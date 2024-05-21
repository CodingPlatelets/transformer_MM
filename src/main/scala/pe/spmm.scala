package pe

import chisel3._
import chisel3.util._
import dataclass.data

// using PE to do num dot vec
class NumDotVec(val bit: Int, val index: Int, val dimV: Int = 32, val numOfMask: Int, val queueSize: Int = 2)
    extends Module {
  val io = IO(new Bundle {
    val num = Flipped(Decoupled(UInt(bit.W)))
    val vec = Flipped(Decoupled((Vec(dimV, UInt(bit.W)))))
    val res = Decoupled(Vec(dimV, UInt(bit.W)))
  })

  val pes = for (i <- 0 until dimV) yield Module(new PE(bit, (index, i), 0))
  for (i <- 0 until dimV) {
    pes(i).io := DontCare
  }

  // generate a result queue with 2 places to store the final result
  val resQueue = Module(new Queue(Vec(dimV, UInt(bit.W)), queueSize, pipe = true, flow = true))
  io.res <> resQueue.io.deq
  resQueue.io.enq.valid := false.B
  resQueue.io.enq.bits := DontCare

  val state = RegInit(State.idle)
  io.num.ready := state === State.send && resQueue.io.enq.ready
  io.vec.ready := state === State.send && resQueue.io.enq.ready

  object State extends ChiselEnum {
    val idle, send = Value
  }

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U(bit.W))))

  val dataValid = WireInit(io.num.valid && io.vec.valid)
  val dataFire = WireInit(io.num.fire && io.vec.fire)

  val produceData = WireInit(dataValid && resQueue.io.enq.ready)

  val (cnt, cntT) = Counter(0 until numOfMask, state === State.send && produceData)

  switch(state) {
    is(State.idle) {
      when(produceData) {
        state := State.send
      }
    }
    is(State.send) {
      when(cnt === 0.U && !dataFire) {
        state := State.idle
      }

      when(cntT) {
        when(produceData) {
          state := State.send
        }.otherwise {
          state := State.idle
          io.num.ready := false.B
          io.vec.ready := false.B
        }

        resQueue.io.enq.valid := true.B
        for (i <- 0 until dimV) {
          resQueue.io.enq.bits(i) := pes(i).io.outReg
        }
      }

      when((!cntT) && dataFire) {
        for (i <- 0 until dimV) {
          pes(i).io.controlSign := ControlSignalSel.SPMM
          pes(i).io.inTop := io.vec.bits(i)
          pes(i).io.inLeft := io.num.bits
          pes(i).io.inReg := Mux(cnt === 0.U, 0.U, tempRegVec(i))
          tempRegVec(i) := pes(i).io.outReg
        }
      }

      // printf("current cnt is %d\n", cnt)
      // printf("current tempRegVec(0) is %d\n", tempRegVec(0))
      // printf("current pes.io.outReg is %d\n", pes(0).io.outReg)
      // printf("current pes.io.inTop is %d\n", pes(0).io.inTop)
      // printf("current pes.io.inLeft is %d\n", pes(0).io.inLeft)
      // printf("current pes.io.inReg is %d\n", pes(0).io.inReg)
      // printf("\n")
    }
  }
}

// spmm using NumDotVec via stream data input
// using mask to choose the needed nums
// to find one's position, using n - n & (n-1) and than one hot to int
// a row of a L x L matrix with mask select the needed nums will dot the specific row of the V matrix selected by the same mask
class spmm(bit: Int = 8, dimV: Int = 32, val L: Int = 32, alu: Int = 1, val numOfMask: Int) extends Module {
  val io = IO(new Bundle {
    val mask = Input(Vec(L, Vec(L, Bool())))
    val numOfMask = Input(UInt(8.W))
    val vMatrix = Input(Vec(L, Vec(dimV, UInt(bit.W))))
    val nums = Flipped(Decoupled(Vec(L, UInt(bit.W))))
    val res = Decoupled(Vec(dimV, UInt(bit.W)))
  })

  io.nums.ready := DontCare
  io.res.valid := DontCare
  io.res.bits := DontCare

  // TODO will generate more ALUs through alu param
  val numDotVec = Module(new NumDotVec(bit, alu, dimV, numOfMask))
  numDotVec.io := DontCare

  val isValid = WireInit(numDotVec.io.res.valid)
  val cnt = utils.counter(L.U - 1.U, isValid)

  object State extends ChiselEnum {
    val idle, calculate, result = Value
  }

  val state = RegInit(State.idle)

  // origin mask number
  val maskReg = RegInit(0.U(L.W))
  val mask1OH = WireInit(0.U(L.W))
  val tempMaskReg = RegInit(VecInit((Seq.fill(L)(VecInit(Seq.fill(L)(false.B))))))
  val tempVMatrixReg = RegInit(VecInit((Seq.fill(L)(VecInit(Seq.fill(dimV)(0.U(bit.W)))))))

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
      //test
      // printf("State is calculate\n")
      // printf("cnt is %d\n", cnt)
      // printf("current numDotVec.num.ready is %d\n", numDotVec.io.num.ready)
      // printf("current numDotVec.vec.ready is %d\n", numDotVec.io.vec.ready)

      when(!numDotVec.io.res.valid && maskReg =/= 0.U) {
        io.nums.ready := true.B
        numDotVec.io.num.valid := true.B
        numDotVec.io.vec.valid := true.B
        io.res.valid := false.B

        mask1OH := utils.maskOH(maskReg)

        numDotVec.io.num.bits := Mux1H(mask1OH, io.nums.bits)
        numDotVec.io.vec.bits := Mux1H(mask1OH, tempVMatrixReg)
        maskReg := maskReg & (maskReg - 1.U)
      }

      when(numDotVec.io.res.valid && cnt < L.U - 1.U) {
        state := State.calculate
        io.res.valid := true.B
        io.res.bits := numDotVec.io.res.bits
        // printf("State is calculate and continue to calculate\n")
        maskReg := Mux1H(UIntToOH(cnt), tempMaskReg).asUInt
        numDotVec.io.res.ready := true.B
      }

      when(cnt === L.U) {
        state := State.result
      }
    }
    // todo: will delete this state
    is(State.result) {
      // printf("State is result and finished\n")
      state := State.idle
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
