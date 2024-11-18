package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fputil.FPMult
import fputil.FPAdd
trait GEMMAccuracyConfig {
  val I: Int = 4
  val F: Int = 12
}

class PEFxp extends Module with GEMMAccuracyConfig with DebugLog {
  val io = IO(new Bundle {
    val in_h = Input(UInt((I + F).W))
    val in_v = Input(UInt((I + F).W))
    val out_h = Output(UInt((I + F).W))
    val out_v = Output(UInt((I + F).W))
    val out = Output(UInt((2 * (I + F)).W))
    val reset = Input(Bool())
  })

  val res = RegInit(0.U((2 * (I + F)).W))

  when(io.reset) {
    res := 0.U
  }.otherwise {
    val tmp = FxpMulPure(io.in_h, io.in_v)(I, F, I, F)
    res := FxpAddPure(res, tmp)(I * 2, F * 2, I * 2, F * 2)
  }

  io.out_h := RegNext(io.in_h)
  io.out_v := RegNext(io.in_v)
  io.out := res
}

// a * b + c
class FMA(width: Int = 32) extends Module with DebugLog {
  val io = IO(new Bundle {
    val a = Input(Valid(UInt(width.W)))
    val b = Input(Valid(UInt(width.W)))
    val c = Input(Valid(UInt(width.W)))
    val out = Valid(UInt(width.W))
  })

  // one cycle latency
  val tmp = FPMult(width)(io.a.bits, io.b.bits, io.a.valid && io.b.valid)

  // three cycle latency
  val tmpRes = FPAdd(width)(io.c.bits, tmp.bits, io.c.valid && tmp.valid)

  io.out.bits := Mux(tmpRes.valid, tmpRes.bits, io.c.bits)
  io.out.valid := tmpRes.valid
}

class PEFp(width: Int = 32, size: Int = 4) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in_h = Input(Valid(UInt(width.W)))
    val in_v = Input(Valid(UInt(width.W)))
    val out_h = Valid(UInt(width.W))
    val out_v = Valid(UInt(width.W))
    val out = Output(UInt(width.W))
    val reset = Input(Bool())
  })

  io.out_h <> RegNext(io.in_h)
  io.out_v <> RegNext(io.in_v)

  val container = RegInit(0.U(width.W))

  val bufferMul = RegInit(VecInit.fill(size)(0.U(width.W)))
  val mulTmp = FPMult(width)(io.in_h.bits, io.in_v.bits, io.in_h.valid && io.in_v.valid)
  val (counter, _) = Counter(0 until size, mulTmp.valid, io.reset)

  bufferMul(counter) := Mux(mulTmp.valid, mulTmp.bits, bufferMul(counter))
  when(io.reset) {
    container := 0.U
  }

  io.out := bufferMul.reduceTree(
    (a, b) => {
      val fadd = Module(new FPAdd(width))
      fadd.io.a.bits := a
      fadd.io.b.bits := b
      fadd.io.a.valid := true.B
      fadd.io.b.valid := true.B
      fadd.io.res.bits
    },
    a => ShiftRegister(a, 3)
  )
  debugLog(p"out: ${io.out}, bufferMul: ${bufferMul}, counter: ${counter}\n", LogLevel.DEBUG)
}

// Compute A * B, where A and B are both square matrix.
class GEMM(val n: Int = 4) extends Module with GEMMAccuracyConfig with DebugLog {

  val InputA = IO(Flipped(Decoupled(Vec(n, Vec(n, UInt((I + F).W))))))
  val InputB = IO(Flipped(Decoupled(Vec(n, Vec(n, UInt((I + F).W))))))
  val OutputPipe = IO(Decoupled(Vec(n * n, UInt((2 * (I + F)).W))))

  // accumulate mode
  val accMode = IO(Input(Bool()))
  val accReg = RegInit(false.B)

  val dataValid = InputA.valid && InputB.valid

  val busy = RegInit(false.B)

  InputA.ready := !busy
  InputB.ready := !busy

  val matrixAReg = RegInit(VecInit.fill(n)(VecInit.fill(n)(0.U((I + F).W))))
  val matrixBReg = RegInit(VecInit.fill(n)(VecInit.fill(n)(0.U((I + F).W))))

  val sysmm = Module(new SystolicMM(n))
  sysmm.io.reset := false.B
  for (i <- 0 until n) {
    sysmm.io.in_a(i) := 0.U
    sysmm.io.in_b(i) := 0.U
  }

  when(dataValid) {
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        matrixAReg(i)(j) := InputA.bits(i)(j)
        matrixBReg(i)(j) := InputB.bits(i)(j)
      }
    }
    busy := true.B
  }

  val resValid = RegInit(false.B)
  OutputPipe.valid := resValid
  OutputPipe.bits := sysmm.io.out

  val cnt = Counter(3 * n)
  when(busy && cnt.value < (2 * n).U) {
    for (i <- 0 until n) {
      val temp = cnt.value >= i.U
      val p = Mux(temp, cnt.value - i.U, 0.U)
      when(temp && p < n.U) {
        sysmm.io.in_a(i) := matrixAReg(i)(p(log2Ceil(n) - 1, 0))
        sysmm.io.in_b(i) := matrixBReg(p(log2Ceil(n) - 1, 0))(i)
      }
      // debugLog(p"in_a${i}: ${sysmm.io.in_a(i)} in_b${i}: ${sysmm.io.in_b(i)}\t")
    }
    debugLog(p"\n")
    cnt.inc()
  }.elsewhen(busy && cnt.value < (3 * n - 1).U) {
    cnt.inc()
  }

  when(cnt.value === (3 * n - 1).U) {
    resValid := true.B
    when(OutputPipe.ready) {
      resValid := false.B
      busy := false.B
      cnt.reset()
      sysmm.io.reset := true.B
    }
  }

  // debugLog(p"busy: ${busy} cnt: ${cnt.value}\n", LogLevel.DEBUG)
}

class SystolicMM(val n: Int = 4) extends Module with GEMMAccuracyConfig with DebugLog {
  val io = IO(new Bundle {
    val in_a = Input(Vec(n, UInt((I + F).W))) // horizontal inputs
    val in_b = Input(Vec(n, UInt((I + F).W))) // vertical inputs
    val out = Output(Vec(n * n, UInt((2 * (I + F)).W)))
    val reset = Input(Bool())
  })

  val p_elems = VecInit(Seq.fill(n * n) { Module(new PEFxp).io })
  for (i <- 0 until n * n) {
    p_elems(i).reset := io.reset
  }

  val h_wires = Wire(Vec((n - 1) * n, UInt((I + F).W)))
  val v_wires = Wire(Vec(n * (n - 1), UInt((I + F).W)))

  def gethidx(r: Int, c: Int): Int = r * (n - 1) + c // last column is terminated
  def getvidx(r: Int, c: Int): Int = r * n + c

  // connecting PEs in a systolic manner
  // debugLog(p"pe(2,0): ${p_elems(8).in_h}, ${p_elems(8).in_v}, ${p_elems(8).out}\n")
  for (col <- 0 until n) {
    for (row <- 0 until n) {
      val pidx = row * n + col
      io.out(pidx) := p_elems(pidx).out // results

      // wiring up PEs
      // horizontal inputs
      if (col == 0) {
        p_elems(pidx).in_h := io.in_a(row)
      } else {
        p_elems(pidx).in_h := h_wires(gethidx(row, col - 1))
      }
      // horizontal outputs to next PEs
      if (col < n - 1) {
        h_wires(gethidx(row, col)) := p_elems(pidx).out_h
      }

      // vertical inputs
      if (row == 0) {
        p_elems(pidx).in_v := io.in_b(col)
      } else {
        p_elems(pidx).in_v := v_wires(getvidx(row - 1, col))
      }
      // vertical outputs to next PEs
      if (row < n - 1) {
        v_wires(getvidx(row, col)) := p_elems(pidx).out_v
      }
    }
  }
}

// each ProcElem (PE) is mapped to each element in a NxN output matrix
class ProcElem(val bits: Int = 8) extends Module {
  val io = IO(new Bundle {
    // input from horizontal direction
    val in_h = Input(UInt(bits.W))
    // input from vertical direction
    val in_v = Input(UInt(bits.W))
    // output to horizontal direction
    val out_h = Output(UInt((bits * 2).W))
    // output to vertical direction
    val out_v = Output(UInt((bits * 2).W))
    // the result after N cycles once this receives the first actual data
    val out = Output(UInt((bits * 2).W))

    val reset = Input(Bool())
  })

  val res = RegInit(0.U((bits * 2).W))

  when(io.reset) {
    res := 0.U
  }
  // this is the main computation part
  res := res + (io.in_h * io.in_v)

  // inputs are delayed one cycle to next PEs
  io.out_h := RegNext(io.in_h)
  io.out_v := RegNext(io.in_v)

  io.out := res
}
