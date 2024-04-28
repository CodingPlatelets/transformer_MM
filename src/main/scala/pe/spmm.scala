package pe

import chisel3._
import chisel3.util._

object utils {
  def counter(max: UInt, cond: Bool) = {
    val x = RegInit(0.U(max.getWidth.W))
    when(cond) {
      x := Mux(x === max, 0.U, x + 1.U)
    }
    x
  }

  def maskOH(mask: Vec[Bool]) = {
    var uMask = mask.asUInt
    uMask - uMask & (uMask - 1.U)
  }
}

// using PE to do num dot vec
class NumDotVec(val bit: Int, val index: Int, val dimV: Int = 32) extends Module {
  val io = IO(new Bundle {
    val num = Flipped(Decoupled(UInt(bit.W)))
    val vec = Flipped(Decoupled((Vec(dimV, UInt(bit.W)))))
    val res = Decoupled(Vec(dimV, UInt((bit * 2).W)))
    val numOfMask = Input(UInt(8.W))
  })

  val pes = for (i <- 0 until dimV) yield Module(new PE(bit, (index, i), 0))
  for (i <- 0 until dimV) {
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

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U((2 * bit).W))))

  val state = RegInit(State.idle)
  val hasData = WireInit(
    state === State.receive && io.num.valid && io.vec.valid
  )
  val cnt = utils.counter(io.numOfMask, hasData)

  switch(state) {
    is(State.idle) {
      for (i <- 0 until dimV) {
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
        for (i <- 0 until dimV) {
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

// spmm using NumDotVec via stream data input
// using mask to choose the needed nums
// to find one's position, using n - n & (n-1) and than one hot to int
// a row of a L x L matrix with mask select the needed nums will dot the specific row of the V matrix selected by the same mask
class spmm(bit: Int = 4, dimV: Int = 32, val L: Int = 32, alu: Int = 1) extends Module {
  val io = IO(new Bundle {
    val mask = Input(Vec(L, Vec(L, Bool())))
    val numOfMask = Input(UInt(8.W))
    val vMatrix = Input(Vec(L, Vec(dimV, UInt(bit.W))))
    val nums = Flipped(Decoupled(Vec(L, UInt(bit.W))))
    val res = Decoupled(Vec(dimV, UInt((2 * bit).W)))
  })

  // TODO: will generate more ALUs through alu param
  var numDotVec = Module(new NumDotVec(bit, alu, dimV))
  val isValid = numDotVec.io.res.valid
  val cnt = utils.counter(L.U, isValid)
  io.nums := DontCare
  io.res := DontCare
  numDotVec.io := DontCare

  object State extends ChiselEnum {
    val idle, calculate, result = Value
  }

  val state = RegInit(State.idle)
  var mask1OH = RegInit(utils.maskOH(io.mask(cnt)))

  switch(state) {
    is(State.idle) {
      io.res.valid := false.B
      io.nums.ready := false.B
      numDotVec.io.num.valid := false.B
      numDotVec.io.vec.valid := false.B

      when(cnt === 0.U && io.nums.valid) {
        state := State.calculate
      }
    }
    is(State.calculate) {
      when(cnt < L.U && io.nums.valid && numDotVec.io.num.ready && numDotVec.io.vec.ready && !numDotVec.io.res.valid) {
        io.nums.ready := true.B
        numDotVec.io.num.valid := true.B
        numDotVec.io.vec.valid := true.B

        numDotVec.io.numOfMask := io.numOfMask
        numDotVec.io.num.bits := Mux1H(mask1OH, io.nums.bits)
        numDotVec.io.vec.bits := Mux1H(mask1OH, io.vMatrix)
        mask1OH := mask1OH & (mask1OH - 1.U)
      }

      when(numDotVec.io.res.valid) {
        state := State.result
      }
    }
    is(State.result) {
      when(cnt < L.U && numDotVec.io.res.valid) {
        numDotVec.io.res.ready := true.B
        io.res.valid := numDotVec.io.res.valid
        io.res.bits := numDotVec.io.res.bits
        state := State.calculate
      }
      when(cnt === L.U) {
        state := State.idle
      }
    }
  }

}
