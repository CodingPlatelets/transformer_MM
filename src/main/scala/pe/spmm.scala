package pe

import chisel3._
import chisel3.util._
import dataclass.data
import os.read

// using PE to do num dot vec
class NumDotVec(val bit: Int, val index: Int, val dimV: Int = 32, val numOfMask: Int, val queueSize: Int = 10)
    extends Module {
  val io = IO(new Bundle {
    val num = Flipped(Decoupled(UInt(bit.W)))
    val vec = Flipped(Decoupled((Vec(dimV, UInt(bit.W)))))
    val res = Decoupled(Vec(dimV, UInt(bit.W)))
    val ready = Output(Bool())
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

  io.ready := resQueue.io.enq.ready

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
class SpMM(bit: Int = 8, dimV: Int = 32, val L: Int = 32, alu: Int = 1, val numOfMask: Int, val queueSize: Int = 10)
    extends Module {
  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val nums = Flipped(Decoupled(Vec(L, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(dimV, UInt(bit.W))))
    val res = Decoupled(Vec(dimV, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))
  })

  // // TODO will generate more ALUs through alu param
  // val numDotVec = Module(new NumDotVec(bit, alu, dimV, numOfMask, queueSize))
  // numDotVec.io.num := DontCare
  // numDotVec.io.vec := DontCare
  // numDotVec.io.res <> io.res

  // val dataValid = io.nums.valid && io.inMask.valid
  // val dataProduce = dataValid && io.res.ready && io.outMask.ready
  // val dataCal = dataValid && numDotVec.io.ready

  // val (cnt, cntT) = Counter(0 until numOfMask, numDotVec.io.num.ready && numDotVec.io.vec.ready)

  // val isConsumed = WireInit(cntT)

  // val firstCycle = RegInit(true.B)

  // when(dataCal) {
  //   io.nums.ready := true.B
  //   io.inMask.ready := true.B
  //   numDotVec.io.num.valid := true.B
  //   numDotVec.io.vec.valid := true.B
  //   numDotVec.io.num.bits := io.nums.bits(maskReg(cnt))
  //   numDotVec.io.vec.bits := Mux(firstCycle, io.vMatrix(maskReg(cnt)), vMatrixReg(maskReg(cnt)))
  //   firstCycle := false.B
  // }

  val numsQueue = Module(new Queue(Vec(L, UInt(bit.W)), queueSize, pipe = true, flow = true))
  val inMaskQueue = Module(new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = true))
  val outMaskQueue = Module(new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = true))
  val resQueue = Module(new Queue(Vec(dimV, UInt(bit.W)), queueSize, pipe = true, flow = true))

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U(bit.W))))
  // val pes = VecInit(Seq.fill(dimV)(Module(new PE(bit, (0, 0), 0)).io))
  val pes = for (i <- 0 until dimV) yield Module(new PE(bit, (0, 0), 0))
  for (i <- 0 until dimV) {
    pes(i).io := DontCare
  }

  numsQueue.io.deq := DontCare
  inMaskQueue.io.deq := DontCare
  outMaskQueue.io.enq := DontCare
  resQueue.io.enq := DontCare

  io.inMask <> inMaskQueue.io.enq
  io.nums <> numsQueue.io.enq
  io.res <> resQueue.io.deq
  io.outMask <> outMaskQueue.io.deq

  val maskReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(utils.maskType.W))))
  val numsReg = RegInit(VecInit(Seq.fill(L)(0.U(bit.W))))
  val vMatrixReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(dimV)(0.U(bit.W))))))
  vMatrixReg := io.vMatrix

  val queueValid = WireInit(inMaskQueue.io.deq.valid && numsQueue.io.deq.valid)
  val queueReady = WireInit(outMaskQueue.io.enq.ready && resQueue.io.enq.ready)
  val isCalculated = RegInit(false.B)

  when(queueValid && queueReady && !isCalculated) {
    maskReg := inMaskQueue.io.deq.bits
    numsReg := numsQueue.io.deq.bits

    numsQueue.io.deq.ready := true.B
    inMaskQueue.io.deq.ready := true.B
    isCalculated := true.B
  }.otherwise {
    inMaskQueue.io.deq.ready := false.B
    numsQueue.io.deq.ready := false.B
  }

  val (cnt, warp) = Counter(0 until numOfMask, isCalculated)
  // printf("current cnt is %d\n", cnt)
  // printf("current tempRegVec(0) is %d\n", tempRegVec(0))
  // printf("current pes.io.outReg is %d\n", pes(0).io.outReg)
  // printf("current pes.io.inTop is %d\n", pes(0).io.inTop)
  // printf("current pes.io.inLeft is %d\n", pes(0).io.inLeft)
  // printf("current pes.io.inReg is %d\n", pes(0).io.inReg)
  // printf("current outQueue Count is %d\n", resQueue.io.count)
  // printf("current inQueue Count is %d\n", numsQueue.io.count)
  // printf("\n")

  when(isCalculated && !warp) {
    outMaskQueue.io.enq.valid := false.B
    resQueue.io.enq.valid := false.B
    for (i <- 0 until dimV) {
      pes(i).io.controlSign := ControlSignalSel.SPMM
      pes(i).io.inTop := io.vMatrix(maskReg(cnt))(i)
      pes(i).io.inLeft := numsReg(maskReg(cnt))
      pes(i).io.inReg := Mux(cnt === 0.U, 0.U, tempRegVec(i))
      tempRegVec(i) := pes(i).io.outReg
    }
  }

  when(warp) {
    resQueue.io.enq.valid := true.B
    for (i <- 0 until dimV) {
      resQueue.io.enq.bits(i) := pes(i).io.outReg
    }
    outMaskQueue.io.enq.bits := maskReg
    outMaskQueue.io.enq.valid := true.B
    isCalculated := false.B
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
