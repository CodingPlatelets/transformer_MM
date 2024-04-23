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

  val tempRegVec = VecInit(Seq.fill(dimQ)(RegInit(0.U((bit * 2).W))))

  val state = RegInit(State.idle)
  val hasData = WireInit(
    state === State.receive && io.num.valid && io.vec.valid
  )
  val cnt = counter(io.numOfMask, hasData)

  switch(state) {
    is(State.idle) {
      io.res.valid := false.B
      io.res.bits := DontCare
      when(cnt === 0.U && io.vec.valid && io.num.valid) {
        state := State.receive
      }
    }
    is(State.receive) {
      io.num.ready := true.B
      io.vec.ready := true.B
      when(io.num.valid && io.vec.valid) {
        for (i <- 0 until dimQ) {
          pes(i).io.controlSign := ControlSignalSel.SPMM
          pes(i).io.inTop := io.vec.bits(i)
          pes(i).io.inLeft := io.num.bits
          pes(i).io.inReg := tempRegVec(i)
          tempRegVec(i) := pes(i).io.outReg
        }
      }
      when(cnt === io.numOfMask) {
        state := State.result
      }.otherwise {
        state := State.receive
      }
    }
    is(State.result) {
      io.num.ready := false.B
      io.vec.ready := false.B
      io.res.valid := true.B
      io.res.bits := tempRegVec
      state := State.idle
      for (i <- 0 until dimQ) {
        tempRegVec(i) := 0.U
      }
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
