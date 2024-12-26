package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fputil.FPMult
import fputil.FPAdd
import hardfloat._

// class SystolicMM(val n: Int = 4, val gemmType: GEMMDataType.Type)(implicit config: DataWidthConfig)
//     extends Module
//     with GEMMAccuracyConfig
//     with DebugLog {

//   val io = IO(new Bundle {
//     val in_a = Input(Vec(n, UInt(config.inputWidth.W)))
//     val in_b = Input(Vec(n, UInt(config.inputWidth.W)))
//     val out = Output(Vec(n * n, UInt(config.outputWidth.W)))
//     val reset = Input(Bool())
//   })

//   val peElements = VecInit(Seq.fill(n * n) {
//     gemmType match {
//       case GEMMDataType.Fxp  => Module(new PEFxp).io
//       case GEMMDataType.Fp32 => Module(new PEFp).io
//       case GEMMDataType.Fp64 => Module(new PEFp).io
//       case _                 => throw new IllegalArgumentException("Unsupported GEMM type")
//     }
//   })

//   peElements.foreach(_.reset := io.reset)

//   val h_wires = Wire(Vec((n - 1) * n, UInt(config.inputWidth.W)))
//   val v_wires = Wire(Vec(n * (n - 1), UInt(config.inputWidth.W)))

//   def gethidx(r: Int, c: Int): Int = r * (n - 1) + c // last column is terminated
//   def getvidx(r: Int, c: Int): Int = r * n + c

//   // connecting PEs in a systolic manner
//   // debugLog(p"pe(2,0): ${p_elems(8).in_h}, ${p_elems(8).in_v}, ${p_elems(8).out}\n")
//   for (col <- 0 until n) {
//     for (row <- 0 until n) {
//       val pidx = row * n + col
//       io.out(pidx) := peElements(pidx).out // results

//       // wiring up PEs
//       // horizontal inputs
//       if (col == 0) {
//         peElements(pidx).in_h := io.in_a(row)
//       } else {
//         peElements(pidx).in_h := h_wires(gethidx(row, col - 1))
//       }
//       // horizontal outputs to next PEs
//       if (col < n - 1) {
//         h_wires(gethidx(row, col)) := peElements(pidx).out_h
//       }

//       // vertical inputs
//       if (row == 0) {
//         peElements(pidx).in_v := io.in_b(col)
//       } else {
//         peElements(pidx).in_v := v_wires(getvidx(row - 1, col))
//       }
//       // vertical outputs to next PEs
//       if (row < n - 1) {
//         v_wires(getvidx(row, col)) := peElements(pidx).out_v
//       }
//     }
//   }
//   // for (row <- 0 until n) {
//   //   for (col <- 0 until n) {
//   //     val pidx = row * n + col
//   //     debugLog(p"${peElements(pidx).out} ", LogLevel.DEBUG)
//   //   }
//   //   debugLog(p"\n", LogLevel.DEBUG)
//   // }
//   // for (col <- 0 until n) {
//   //   for (row <- 0 until n) {
//   //     val pidx = row * n + col
//   //     debugLog(p"${io.out(pidx)} ", LogLevel.DEBUG)
//   //   }
//   //   debugLog(p"\n", LogLevel.DEBUG)
//   // }
// }

// Compute A * B = C, where A is p*m, B is m*q, C is p*q 
class CustomGemm (val n: Int = 4, val p: Int = 512, val q: Int = 512, val m: Int = 128, val gemmType: GEMMDataType.Type) (implicit config: DataWidthConfig)
    extends Module
    with GEMMAccuracyConfig
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(p * m, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(m * q, UInt(config.inputWidth.W))))
    val out = Decoupled(Vec(p * q, UInt(config.outputWidth.W)))
    val reset = Input(Bool())
  })
  val matrixAReg = RegInit(VecInit.fill(p)(VecInit.fill(m)(0.U(config.inputWidth.W))))
  val matrixBReg = RegInit(VecInit.fill(m)(VecInit.fill(q)(0.U(config.inputWidth.W))))
  val ResultReg = RegInit(VecInit.fill(p)(VecInit.fill(q)(0.U(config.inputWidth.W))))

  val dataValid = io.in_a.valid && io.in_b.valid

  val busy = RegInit(false.B)

  io.in_a.ready := !busy
  io.in_b.ready := !busy

  val sysmm = Module(new SystolicMM(n, gemmType))
  sysmm.io.reset := false.B
  for (i <- 0 until n) {
    sysmm.io.in_a(i) := 0.U
    sysmm.io.in_b(i) := 0.U
  }

  when(dataValid) {
    for (i <- 0 until m) {
      for (j <- 0 until p) {
        matrixAReg(j)(i) := io.in_a.bits(j * m + i)
      }
      for (j <- 0 until q) {
        matrixBReg(i)(j) := io.in_b.bits(i * q + j)
      }
    }
    busy := true.B
  }

  val resValid = RegInit(false.B)
  io.out.valid := resValid
  for (i <- 0 until p) {
    for (j <- 0 until q) {
      io.out.bits(i * q + j) := ResultReg(i)(j)
    }
  }
  val rowCnt = p / n
  val colCnt = q / n
  val rowIndex = Counter(rowCnt + 1)
  val colIndex = Counter(colCnt + 1)
  when(rowIndex.value < rowCnt.U){
    when(colIndex.value < colCnt.U){
      val cnt = Counter(m + 2 * n)
      when(busy && cnt.value < (n + m).U) {
        for (i <- 0 until n) {
          val temp = cnt.value >= i.U
          val p = Mux(temp, cnt.value - i.U, 0.U)
          val row = rowIndex.value * n.U + i.U
          val col = colIndex.value * n.U + i.U
          when(temp && p < m.U) {
            sysmm.io.in_a(i) := matrixAReg(row(log2Ceil(rowCnt), 0))(p(log2Ceil(m) - 1, 0))
            sysmm.io.in_b(i) := matrixBReg(p(log2Ceil(m) - 1, 0))(col(log2Ceil(colCnt), 0))
          }
        }
        debugLog(p"\n")
        cnt.inc()
      }.elsewhen(busy && cnt.value < (m + 2 * n - 1).U) {
        cnt.inc()
      }
      when(cnt.value === (m + 2 * n - 1).U) {
        cnt.reset()
        colIndex.inc()
        for (i <- 0 until n) {
          for (j <- 0 until n) {
            val row = rowIndex.value * n.U + i.U
            val col = colIndex.value * n.U + j.U
            debugLog(p"row: ${row} col: ${col} value: ${sysmm.io.out(i * n + j)} \n", LogLevel.DEBUG)
            ResultReg(row)(col) := sysmm.io.out(i * n + j)
          }
        }
        sysmm.io.reset := true.B
      }
    }
    when(colIndex.value === colCnt.U) {
      colIndex.inc()
      rowIndex.inc()
    }
  }
  when(rowIndex.value === rowCnt.U) {
    resValid := true.B
    when(io.out.ready) {
      rowIndex.reset()
      colIndex.reset()
      resValid := false.B
      busy := false.B
    }
  }
  //println(cf"row is ${rowIndex} col is ${colIndex}")
  //printf(p"${rowIndex.value === rowCnt.U}\n")
  //debugLog(p"busy: ${busy} row: ${rowIndex.value} col: ${colIndex.value} out_valid: ${io.out.valid}\n", LogLevel.DEBUG)
}
