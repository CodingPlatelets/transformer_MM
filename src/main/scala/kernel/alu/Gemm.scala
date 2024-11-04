package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog

// Compute A * B, where A and B are both square matrix.
class GEMM(val n: Int = 4, val bits: Int = 8) extends Module with DebugLog {

  val InputA = IO(Input(Vec(n, Vec(n, UInt(bits.W)))))
  val InputB = IO(Input(Vec(n, Vec(n, UInt(bits.W)))))
  val DataReady = IO(Input(Bool()))
  val OutputPipe = IO(Decoupled(Vec(n * n, UInt((bits * 2).W))))

  val sysmm = Module(new SystolicMM(n, bits))
  val resValid = RegInit(false.B)
  OutputPipe.valid := resValid
  OutputPipe.bits := sysmm.io.out

  for (i <- 0 until n) {
    sysmm.io.in_a(i) := 0.U
    sysmm.io.in_b(i) := 0.U
  }
  val cnt = Counter(3 * n)
  when(DataReady && cnt.value < (2 * n).U) {
    for (i <- 0 until n) {
      val temp = cnt.value >= i.U
      val p = Mux(temp, cnt.value - i.U, 0.U)
      when(temp && p < n.U) {
        sysmm.io.in_a(i) := InputA(i)((p)(log2Ceil(n) - 1, 0))
        sysmm.io.in_b(i) := InputB((p)(log2Ceil(n) - 1, 0))(i)
      }
      // debugLog(p"in_a${i}: ${sysmm.io.in_a(i)} in_b${i}: ${sysmm.io.in_b(i)}\t")
    }
    debugLog(p"\n")
    cnt.inc()
  }.elsewhen(DataReady && cnt.value < (3 * n - 1).U) {
    cnt.inc()
  }

  when(cnt.value === (3 * n - 1).U) {
    resValid := true.B
    when(OutputPipe.ready) {
      resValid := false.B
      cnt.reset()
    }
  }
}

class SystolicMM(val n: Int = 4, val bits: Int = 8) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in_a = Input(Vec(n, UInt(bits.W))) // horizontal inputs
    val in_b = Input(Vec(n, UInt(bits.W))) // vertical inputs
    val out = Output(Vec(n * n, UInt((bits * 2).W)))
  })

  val p_elems = VecInit(Seq.fill(n * n) { Module(new ProcElem(bits)).io })
  val h_wires = Wire(Vec((n - 1) * n, UInt(bits.W)))
  val v_wires = Wire(Vec(n * (n - 1), UInt(bits.W)))

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
  })

  val res = RegInit(0.U((bits * 2).W))
  // this is the main computation part
  res := res + (io.in_h * io.in_v)

  // inputs are delayed one cycle to next PEs
  io.out_h := RegNext(io.in_h)
  io.out_v := RegNext(io.in_v)

  io.out := res
}
