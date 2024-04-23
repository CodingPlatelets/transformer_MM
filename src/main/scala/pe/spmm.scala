package pe

import chisel3._
import chisel3.util._

// using PE to do num dot vec
class NumDotVec(val bit: Int, val index: Int, val dimQ: Int = 32)
    extends Module {
  val io = IO(new Bundle {
    val num = Flipped(Decoupled(UInt(bit.W)))
    val vec = Flipped(Decoupled((Vec(dimQ, UInt(bit.W)))))
    val res = Decoupled(Vec(dimQ, UInt((bit * 2).W)))
    val numOfMask = Input(UInt(8.W))
  })

  def counter(max: UInt, cond: Bool) = {
    val x = RegInit(0.U(max.getWidth.W))
    when(cond) {
      x := Mux(x === max, 0.U, x + 1.U)
    }
    x
  }

  val tempRegVec = VecInit(Seq.fill(dimQ)(RegInit(0.U((bit * 2).W))))
  val hasData = WireInit(tempRegVec.asUInt =/= 0.U)
  val cnt = counter(io.numOfMask - 1.U, hasData)

  when(cnt === 0.U) {
    for (i <- 0 until dimQ) {
      tempRegVec(i) := 0.U
    }
  }

  // a logic that when res is valid then num and vec are ready,
  // and when num and vec are not ready then res is not valid
  io.res.valid := false.B
  io.res.bits := DontCare
  io.num.ready := io.res.valid && io.res.ready
  io.vec.ready := io.res.valid && io.res.ready
  when(io.res.ready && cnt === io.numOfMask - 1.U) {
    io.res.valid := true.B
    io.res.bits := tempRegVec
  }

  when(io.num.ready && io.num.valid && io.vec.ready && io.vec.valid) {
    for (i <- 0 until dimQ) {
      val pe = Module(new PE(bit, (index, i), 0))
      pe.io.controlSign := ControlSignalSel.SPMM
      pe.io.inTop := io.vec.bits(i)
      pe.io.inLeft := io.num.bits
      pe.io.inReg := tempRegVec(i)
      tempRegVec(i) := pe.io.outReg
    }
  }

}

// spmm using PE
// using mask to choose the needed nums
class spmm(dimSpare: Int, bit: Int = 4, val dimV: Int = 32) extends Module {
  val io = IO(new Bundle {
    val spareNums = Input(Vec(dimSpare, UInt(bit.W)))
    val vNums = Input(Vec(dimV, UInt(bit.W)))

  })

}
