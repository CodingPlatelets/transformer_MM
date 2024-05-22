package pe

import chisel3._
import chisel3.util._

class VecDotVecTree(val dim: Int) extends Module {

  val io = IO(new Bundle {
    val rowQ = Input(Vec(dim, UInt(8.W)))
    val rowK = Input(Vec(dim, UInt(8.W)))
    val res = Output(UInt((16 + dim).W))
  })

  // multiply each element of rowQ with each element of rowK and sum them using Vec.reduceTree function
  val sumMultiply = VecInit((io.rowQ.zip(io.rowK)).map { case (a, b) => a * b })
  io.res := sumMultiply.reduceTree((a, b) => RegNext(a +& b))

}

class VecDotVec(val index: Int = 0, val bit: Int, val D: Int) extends Module {
  val io = IO(new Bundle {
    val rowQ = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val colK = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val res = Decoupled(UInt(bit.W))
  })
  // using D cycle to calculate the result
  val tempReg = RegInit(0.U(bit.W))
  val pe = Module(new PE(bit, (index, 0), 0))
  pe.io := DontCare
  io.rowQ.ready := DontCare
  io.colK.ready := DontCare
  io.res := DontCare
  val temQ = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))
  val temK = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))

  object State extends ChiselEnum {
    val idle, calculate, result = Value
  }

  val state = RegInit(State.idle)

  val (cnt, cntFull) = util.Counter(0 until D, state === State.calculate)

  io.rowQ.ready := Mux(cnt === 0.U, true.B, false.B)
  io.colK.ready := Mux(cnt === 0.U, true.B, false.B)
  temQ := io.rowQ.bits
  temK := io.colK.bits

  io.res.bits := tempReg

  when(io.res.valid && io.res.ready) {
    io.res.valid := false.B
  }

  switch(state) {
    is(State.idle) {
      io.res.valid := false.B
      when(io.colK.valid && io.rowQ.valid) {
        state := State.calculate
      }
    }

    is(State.calculate) {
      // printf("cnt is %d\n", cnt)
      when(cntFull) {
        when(io.rowQ.valid && io.colK.valid) {
          state := State.calculate
        }.otherwise {
          state := State.result
        }
        io.res.valid := true.B
        io.res.bits := pe.io.outReg
        tempReg := pe.io.outReg
      }

      when(!cntFull) {
        io.res.valid := false.B
        pe.io.controlSign := ControlSignalSel.SDDMM
        pe.io.inLeft := Mux(cnt === 0.U, io.rowQ.bits(cnt), temQ(cnt))
        pe.io.inTop := Mux(cnt === 0.U, io.colK.bits(cnt), temK(cnt))
        pe.io.inReg := Mux(cnt === 0.U, 0.U, tempReg)
        tempReg := pe.io.outReg
      }

      printf(
        "pe(%d) in cycle %d, left is %d, top is %d, temReg is %d\n",
        index.U,
        cnt,
        pe.io.inLeft,
        pe.io.inTop,
        tempReg
      )
    }

    is(State.result) {
      // printf("result\n")
      when(io.rowQ.valid && io.colK.valid) {
        state := State.calculate
      }

      when(!io.res.ready) {
        state := State.idle
      }

      io.res.valid := true.B
      io.res.bits := tempReg
    }
  }
}

// todo: cannot create pes dynamically, so we must fix the numOfMask, but we can set a min numOfMask and schedule it in the future
class sddmm(bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4, val queueSize: Int = 10)
    extends Module {
  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val qVec = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    // L x D matrix for row dot row
    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(L, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))
  })

  val qVecQueue = Module(new Queue(Vec(D, UInt(bit.W)), queueSize, pipe = true, flow = true))
  val inMaskQueue = Module(new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = true))
  val outMaskQueue = Module(new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = true))
  val resQueue = Module(new Queue(Vec(L, UInt(bit.W)), queueSize, pipe = true, flow = true))

  io.inMask <> inMaskQueue.io.enq
  io.qVec <> qVecQueue.io.enq
  io.outMask <> outMaskQueue.io.deq
  io.res <> resQueue.io.deq

  inMaskQueue.io.deq := DontCare
  qVecQueue.io.deq := DontCare
  outMaskQueue.io.enq := DontCare
  resQueue.io.enq := DontCare

  val pes = for (i <- 0 until numOfMask) yield Module(new PE(bit, (i, 0), 0))
  for (i <- 0 until numOfMask) {
    pes(i).io := DontCare
  }
  val tempReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(bit.W))))
  val tempK = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
  tempK := io.kMatrix

  val dataValid = WireInit(inMaskQueue.io.deq.valid && qVecQueue.io.deq.valid)
  val dataReady = WireInit(outMaskQueue.io.enq.ready && resQueue.io.enq.ready)

  val tempQ = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))
  val tempMask = RegInit(VecInit(Seq.fill(numOfMask)(0.U(utils.maskType.W))))

  val isCalculated = RegInit(false.B)
  when(dataReady && dataValid && !isCalculated) {
    tempQ := qVecQueue.io.deq.bits
    tempMask := inMaskQueue.io.deq.bits
    qVecQueue.io.deq.ready := true.B
    inMaskQueue.io.deq.ready := true.B
    isCalculated := true.B
  }.otherwise {
    inMaskQueue.io.deq.ready := false.B
    qVecQueue.io.deq.ready := false.B
  }

  val (cnt, warp) = Counter(0 until D, isCalculated)

  when(isCalculated && !warp) {
    outMaskQueue.io.enq.valid := false.B
    resQueue.io.enq.valid := false.B
    for (i <- 0 until numOfMask) {
      pes(i).io.inLeft := tempQ(cnt)
      pes(i).io.inTop := tempK(tempMask(i))(cnt)
      pes(i).io.inReg := Mux(cnt === 0.U, 0.U, tempReg(i))
      pes(i).io.controlSign := ControlSignalSel.SDDMM
      tempReg(i) := pes(i).io.outReg
    }
  }

  when(warp) {
    for (i <- 0 until numOfMask) {
      resQueue.io.enq.bits(tempMask(i)) := pes(i).io.outReg
    }
    outMaskQueue.io.enq.bits := tempMask
    resQueue.io.enq.valid := true.B
    outMaskQueue.io.enq.valid := true.B
    isCalculated := false.B
  }

  // printf("current cnt is %d\n", cnt)
  // printf("current tempRegVec(0) is %d\n", tempReg(0))
  // printf("current pes.io.outReg is %d\n", pes(0).io.outReg)
  // printf("current pes.io.inTop is %d\n", pes(0).io.inTop)
  // printf("current pes.io.inLeft is %d\n", pes(0).io.inLeft)
  // printf("current pes.io.inReg is %d\n", pes(0).io.inReg)
  // printf("current outQueue Count is %d\n", resQueue.io.count)
  // printf("current inQueue Count is %d\n", qVecQueue.io.count)
  // printf("\n")

}
