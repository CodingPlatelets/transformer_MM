package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fputil.FPMult
import fputil.FPAdd
import hardfloat._

trait GEMMAccuracyConfig {
  val I: Int = 8
  val F: Int = 0
}

case class FPConfig(width: Int) {
  private val fpParams = Map(
    32 -> (8, 24), // 单精度
    64 -> (11, 53) // 双精度
  )
  val (expWidth, sigWidth) =
    fpParams.getOrElse(width, throw new IllegalArgumentException(s"Unsupported floating point width: $width"))
}

object GEMMDataType extends ChiselEnum {
  // UInt, FixedPoint, FloatPoint(32), FloatPoint(64)
  val UI, Fxp, Fp32, Fp64 = Value
}

trait DataWidthConfig {
  def inputWidth:  Int
  def outputWidth: Int
}

case object FxpConfig extends DataWidthConfig with GEMMAccuracyConfig {
  def inputWidth:  Int = I + F
  def outputWidth: Int = I + F
}

case object Fp32Config extends DataWidthConfig {
  def inputWidth:  Int = 32
  def outputWidth: Int = 32
}

case object Fp64Config extends DataWidthConfig {
  def inputWidth:  Int = 64
  def outputWidth: Int = 64
}

class PEFxp(implicit config: DataWidthConfig) extends Module with GEMMAccuracyConfig with DebugLog {
  val io = IO(new Bundle {
    val in_h = Input(UInt(config.inputWidth.W))
    val in_v = Input(UInt(config.inputWidth.W))
    val out_h = Output(UInt(config.inputWidth.W))
    val out_v = Output(UInt(config.inputWidth.W))
    val out = Output(UInt(config.outputWidth.W))
    val reset = Input(Bool())
  })

  val res = RegInit(0.U(config.outputWidth.W))

  when(io.reset) {
    res := 0.U
  }.otherwise {
    val tmp = FxpMulPure(io.in_h, io.in_v)(I, F, I, F)
    res := FxpAddPure(res, tmp)(I, F, I, F)
  }

  io.out_h := RegNext(io.in_h)
  io.out_v := RegNext(io.in_v)
  io.out := res
}

class PEFp(implicit config: DataWidthConfig) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in_h = Input(UInt(config.inputWidth.W))
    val in_v = Input(UInt(config.inputWidth.W))
    val out_h = Output(UInt(config.inputWidth.W))
    val out_v = Output(UInt(config.inputWidth.W))
    val out = Output(UInt(config.outputWidth.W))
    val reset = Input(Bool())
  })

  io.out_h := RegNext(io.in_h)
  io.out_v := RegNext(io.in_v)

  val res = RegInit(0.U(config.inputWidth.W))
  val fpConfig = FPConfig(config.inputWidth)
  val FCMAModule = Module(new fudian.FCMA(fpConfig.expWidth, fpConfig.sigWidth))
  FCMAModule.io.a := io.in_h
  FCMAModule.io.b := io.in_v
  FCMAModule.io.c := res
  FCMAModule.io.rm := 0.U
  res := Mux(io.reset, 0.U, FCMAModule.io.result)
  io.out := res
  FCMAModule.io.fflags := DontCare
}

class SystolicMM(val n: Int = 4, val gemmType: GEMMDataType.Type)(implicit config: DataWidthConfig)
    extends Module
    with GEMMAccuracyConfig
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Input(Vec(n, UInt(config.inputWidth.W)))
    val in_b = Input(Vec(n, UInt(config.inputWidth.W)))
    val out = Output(Vec(n * n, UInt(config.outputWidth.W)))
    val reset = Input(Bool())
  })

  val peElements = VecInit(Seq.fill(n * n) {
    gemmType match {
      case GEMMDataType.Fxp  => Module(new PEFxp).io
      case GEMMDataType.Fp32 => Module(new PEFp).io
      case GEMMDataType.Fp64 => Module(new PEFp).io
      case _                 => throw new IllegalArgumentException("Unsupported GEMM type")
    }
  })

  peElements.foreach(_.reset := io.reset)

  val h_wires = Wire(Vec((n - 1) * n, UInt(config.inputWidth.W)))
  val v_wires = Wire(Vec(n * (n - 1), UInt(config.inputWidth.W)))

  def gethidx(r: Int, c: Int): Int = r * (n - 1) + c // last column is terminated
  def getvidx(r: Int, c: Int): Int = r * n + c

  // connecting PEs in a systolic manner
  // debugLog(p"pe(2,0): ${p_elems(8).in_h}, ${p_elems(8).in_v}, ${p_elems(8).out}\n")
  for (col <- 0 until n) {
    for (row <- 0 until n) {
      val pidx = row * n + col
      io.out(pidx) := peElements(pidx).out // results

      // wiring up PEs
      // horizontal inputs
      if (col == 0) {
        peElements(pidx).in_h := io.in_a(row)
      } else {
        peElements(pidx).in_h := h_wires(gethidx(row, col - 1))
      }
      // horizontal outputs to next PEs
      if (col < n - 1) {
        h_wires(gethidx(row, col)) := peElements(pidx).out_h
      }

      // vertical inputs
      if (row == 0) {
        peElements(pidx).in_v := io.in_b(col)
      } else {
        peElements(pidx).in_v := v_wires(getvidx(row - 1, col))
      }
      // vertical outputs to next PEs
      if (row < n - 1) {
        v_wires(getvidx(row, col)) := peElements(pidx).out_v
      }
    }
  }
}

// Compute A * B, where A and B are both square matrix.
class GEMM(val n: Int = 4, val gemmType: GEMMDataType.Type)(implicit config: DataWidthConfig)
    extends Module
    with GEMMAccuracyConfig
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(n * n, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(n * n, UInt(config.inputWidth.W))))
    val out = Decoupled(Vec(n * n, UInt(config.outputWidth.W)))
    val reset = Input(Bool())
  })

  val dataValid = io.in_a.valid && io.in_b.valid

  val busy = RegInit(false.B)

  io.in_a.ready := !busy
  io.in_b.ready := !busy

  val matrixAReg = RegInit(VecInit.fill(n)(VecInit.fill(n)(0.U(config.inputWidth.W))))
  val matrixBReg = RegInit(VecInit.fill(n)(VecInit.fill(n)(0.U(config.inputWidth.W))))

  val sysmm = Module(new SystolicMM(n, gemmType))
  sysmm.io.reset := false.B
  for (i <- 0 until n) {
    sysmm.io.in_a(i) := 0.U
    sysmm.io.in_b(i) := 0.U
  }

  when(dataValid) {
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        matrixAReg(i)(j) := io.in_a.bits(i * n + j)
        matrixBReg(i)(j) := io.in_b.bits(i * n + j)
      }
    }
    busy := true.B
  }

  val resValid = RegInit(false.B)
  io.out.valid := resValid
  io.out.bits := sysmm.io.out

  val cnt = Counter(3 * n)
  when(busy && cnt.value < (2 * n).U) {
    for (i <- 0 until n) {
      val temp = cnt.value >= i.U
      val p = Mux(temp, cnt.value - i.U, 0.U)
      when(temp && p < n.U) {
        sysmm.io.in_a(i) := matrixAReg(i)(p(log2Ceil(n) - 1, 0))
        sysmm.io.in_b(i) := matrixBReg(p(log2Ceil(n) - 1, 0))(i)
      }
      debugLog(p"in_a${i}: ${sysmm.io.in_a(i)} in_b${i}: ${sysmm.io.in_b(i)}\t")
    }
    debugLog(p"\n")
    cnt.inc()
  }.elsewhen(busy && cnt.value < (3 * n - 1).U) {
    cnt.inc()
  }

  when(cnt.value === (3 * n - 1).U) {
    resValid := true.B
    // debugLog(p"res: ${sysmm.io.out}\n", LogLevel.DEBUG)
    when(resValid && io.out.ready) {
      resValid := false.B
      busy := false.B
      cnt.reset()
      sysmm.io.reset := io.reset
    }
  }

  // debugLog(p"busy: ${busy} resValid: ${resValid} cnt: ${cnt.value}\n", LogLevel.DEBUG)
}
