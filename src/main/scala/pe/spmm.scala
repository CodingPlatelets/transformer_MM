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

  // this will find the last "one" in an UInt, and then convert it to a one hot num
  def maskOH(mask: UInt) = {
    mask - (mask & (mask - 1.U))
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
        tempRegVec(i) := 0.U
      }
      io.res.valid := false.B
      when(cnt === 0.U) {
        state := State.receive
      }
    }
    is(State.receive) {
      io.num.ready := true.B
      io.vec.ready := true.B
      io.res.valid := false.B

      // TODO: maybe it should not wait for a cycle
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
      io.num.ready := false.B
      io.vec.ready := false.B
      io.res.valid := true.B
      io.res.bits := tempRegVec

      state := State.idle
    }
  }
}

// spmm using NumDotVec via stream data input
// using mask to choose the needed nums
// to find one's position, using n - n & (n-1) and than one hot to int
// a row of a L x L matrix with mask select the needed nums will dot the specific row of the V matrix selected by the same mask
class spmm(bit: Int = 8, dimV: Int = 32, val L: Int = 32, alu: Int = 1) extends Module {
  val io = IO(new Bundle {
    val mask = Input(Vec(L, Vec(L, Bool())))
    val numOfMask = Input(UInt(8.W))
    val vMatrix = Input(Vec(L, Vec(dimV, UInt(bit.W))))
    val nums = Flipped(Decoupled(Vec(L, UInt(bit.W))))
    val res = Decoupled(Vec(dimV, UInt((2 * bit).W)))
  })

  io.nums.ready := DontCare
  io.res.valid := DontCare
  io.res.bits := DontCare

  // TODO will generate more ALUs through alu param
  val numDotVec = Module(new NumDotVec(bit, alu, dimV))
  numDotVec.io := DontCare

  val isValid = WireInit(numDotVec.io.res.valid)
  val cnt = utils.counter(L.U, isValid)

  object State extends ChiselEnum {
    val idle, calculate, result = Value
  }

  val state = RegInit(State.idle)

  // origin mask number
  val maskReg = RegInit(0.U(L.W))
  val mask1OH = WireInit(0.U(L.W))
  val tempMaskReg = RegInit(VecInit((Seq.fill(L)(VecInit(Seq.fill(L)(false.B))))))
  val tempVMatrixReg = RegInit(VecInit((Seq.fill(L)(VecInit(Seq.fill(dimV)(0.U(bit.W)))))))

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U((2 * bit).W))))
  switch(state) {
    is(State.idle) {
      // test
      // printf("State is idle\n")

      io.res.valid := false.B
      io.nums.ready := false.B
      numDotVec.io.num.valid := false.B
      numDotVec.io.vec.valid := false.B
      maskReg := Mux1H(UIntToOH(cnt), io.mask).asUInt
      tempMaskReg := io.mask
      tempVMatrixReg := io.vMatrix

      when(cnt === 0.U && io.nums.valid && numDotVec.io.num.ready && numDotVec.io.vec.ready) {
        state := State.calculate
      }
    }
    is(State.calculate) {
      // //test
      // printf("State is calculate\n")
      // printf("cnt is %d\n", cnt)
      // printf("current numDotVec.num.ready is %d\n", numDotVec.io.num.ready)
      // printf("current numDotVec.vec.ready is %d\n", numDotVec.io.vec.ready)
      io.res.valid := false.B

      when(!numDotVec.io.res.valid && maskReg =/= 0.U) {
        io.nums.ready := true.B
        numDotVec.io.num.valid := true.B
        numDotVec.io.vec.valid := true.B

        numDotVec.io.numOfMask := io.numOfMask
        mask1OH := utils.maskOH(maskReg)

        numDotVec.io.num.bits := Mux1H(mask1OH, io.nums.bits)
        numDotVec.io.vec.bits := Mux1H(mask1OH, tempVMatrixReg)
        maskReg := maskReg & (maskReg - 1.U)
      }

      // TODO will wait for numDotVec's last cycle to calculate the last result, so it will wait for a more cycle
      when(numDotVec.io.res.valid) {
        state := State.result
        tempRegVec := numDotVec.io.res.bits
      }
    }
    is(State.result) {
      when(cnt < L.U) {
        maskReg := Mux1H(UIntToOH(cnt), tempMaskReg).asUInt
        // printf("State is result and continue to calculate\n")
        numDotVec.io.res.ready := true.B
        io.res.valid := true.B
        io.res.bits := tempRegVec
        state := State.calculate
      }
      when(cnt === L.U) {
        // printf("State is result and finished\n")
        state := State.idle
      }
    }
  }

}

// test for mux1h
class testMux extends Module {
  val nums = IO(Flipped(Decoupled(Vec(16, Vec(16, UInt(4.W))))))
  val sel = IO(Input(Vec(16, Bool())))
  val out = IO(Decoupled(Vec(16, UInt(4.W))))

  nums.ready := true.B
  out.valid := true.B
  val res = Mux1H(sel.asUInt, nums.bits)
  out.bits := res
}

class counterMux1H(val L: Int = 8) extends Module {
  val counter = IO(Input(UInt(L.W)))
  val vec = IO(Input(Vec(L, Vec(L, Bool()))))

  val out = IO(Output(Vec(L, Bool())))

  out := Mux1H(UIntToOH(counter), vec)
}

class findOneHot extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))

  out := in - (in & (in - 1.U))
}
