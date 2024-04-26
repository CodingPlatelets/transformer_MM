package pe

import chisel3._
import chisel3.util._

// using PE to do num dot vec
class NumDotVec(val bit: Int, val index: Int, val dimQ: Int = 32) extends Module {
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

  val pes = for (i <- 0 until dimQ) yield Module(new PE(bit, (index, i), 0))
  for (i <- 0 until dimQ) {
    pes(i).io := DontCare
  }

  io.res := DontCare
  io.vec.ready := DontCare
  io.num.ready := DontCare

  /** Generate a FSM to receive all num and vec in n cycles and then use PE to
    * calculate the result.
    *
    * The rules are as follows:
    *
    * idle: when counter = 0 turn state to receive.
    *
    * receive: when counter < numOfMask: turn num and vec to ready then receive
    * their data when counter = numOfMask: turn the state -> result
    *
    * result: turn the num and vec's ready to false and turn the res.valid to
    * true and res.bits to the result then turn the state to idle
    */

  object State extends ChiselEnum {
    val idle, receive, result = Value
  }

  val tempRegVec = RegInit(VecInit(Seq.fill(dimQ)(0.U((2 * bit).W))))

  val state = RegInit(State.idle)
  val hasData = WireInit(
    state === State.receive && io.num.valid && io.vec.valid
  )
  val cnt = counter(io.numOfMask, hasData)

  switch(state) {
    is(State.idle) {
      for (i <- 0 until dimQ) {
        tempRegVec(i) := WireInit(0.U)
      }
      io.res.valid := WireInit(false.B)
      when(cnt === 0.U && io.vec.valid && io.num.valid) {
        state := State.receive
      }
    }
    is(State.receive) {
      io.num.ready := WireInit(true.B)
      io.vec.ready := WireInit(true.B)
      io.res.valid := WireInit(false.B)

      when(cnt === io.numOfMask) {
        state := State.result
      }

      when(cnt < io.numOfMask && io.num.valid && io.vec.valid) {
        for (i <- 0 until dimQ) {
          pes(i).io.controlSign := ControlSignalSel.SPMM
          pes(i).io.inTop := WireInit(io.vec.bits(i))
          pes(i).io.inLeft := WireInit(io.num.bits)
          pes(i).io.inReg := WireInit(tempRegVec(i))
          tempRegVec(i) := pes(i).io.outReg
        }
      }
    }
    is(State.result) {

      io.num.ready := WireInit(false.B)
      io.vec.ready := WireInit(false.B)
      io.res.valid := WireInit(true.B)
      io.res.bits := tempRegVec

      state := State.idle
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
