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


// todo: cannot create pes dynamically, so we must fix the numOfMask, but we can set a min numOfMask and schedule it in the future
class Sddmm(bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4, val queueSize: Int = 2)
    extends Module {
  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val qVec = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    // L x D matrix for row dot row
    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(L, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))
  })

  val qVecQueue = Module(new Queue(Vec(D, UInt(bit.W)), queueSize, pipe = true, flow = false, useSyncReadMem = false))
  val inMaskQueue = Module(
    new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = false, useSyncReadMem = false)
  )

  io.inMask <> inMaskQueue.io.enq
  io.qVec <> qVecQueue.io.enq

  inMaskQueue.io.deq.bits := DontCare
  qVecQueue.io.deq.bits := DontCare

  val pes = for (i <- 0 until numOfMask) yield Module(new PE(bit, (i, 0), 0))
  for (i <- 0 until numOfMask) {
    pes(i).io := DontCare
  }
  val tempReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(bit.W))))
  val tempK = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
  tempK := io.kMatrix

  val dataValid = WireInit(inMaskQueue.io.deq.valid && qVecQueue.io.deq.valid)

  val tempQ = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))
  val tempMask = RegInit(VecInit(Seq.fill(numOfMask)(0.U(utils.maskType.W))))

  val busy = RegInit(false.B)
  inMaskQueue.io.deq.ready := !busy
  qVecQueue.io.deq.ready := !busy

  val resValid = RegInit(false.B)
  io.res.valid := resValid
  io.outMask.valid := resValid
  io.res.bits := DontCare
  io.outMask.bits := DontCare
  val finishedButNoAccepted = RegInit(false.B)

  val (cnt, warp) = Counter(0 until D, busy & (!resValid) & !(finishedButNoAccepted))

  when(busy) {
    when(!warp && !finishedButNoAccepted) {
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
        tempReg(i) := pes(i).io.outReg
        // printf("(%d) is %d\t", i.U, pes(i).io.outReg)
      }
      // printf("\n")
      finishedButNoAccepted := true.B
    }

    when(finishedButNoAccepted) {
      for (i <- 0 until numOfMask) {
        io.res.bits(tempMask(i)) := tempReg(i)
      }
      io.outMask.bits := tempMask
      resValid := true.B
      when(resValid && io.res.ready && io.outMask.ready) {
        resValid := false.B
        finishedButNoAccepted := false.B
        busy := false.B
      }
    }

  }.otherwise {
    resValid := false.B
    when(dataValid) {
      tempQ := qVecQueue.io.deq.bits
      tempMask := inMaskQueue.io.deq.bits
      qVecQueue.io.deq.ready := true.B
      inMaskQueue.io.deq.ready := true.B
      busy := true.B
    }
  }

  // printf("current cnt is %d\n", cnt)
  // printf("current tempRegVec(0) is %d\n", tempReg(0))
  // printf("current pes.io.outReg is %d\n", pes(0).io.outReg)
  // printf("current pes.io.inTop is %d\n", pes(0).io.inTop)
  // printf("current pes.io.inLeft is %d\n", pes(0).io.inLeft)
  // printf("current pes.io.inReg is %d\n", pes(0).io.inReg)
  // printf("current inQueue Count is %d\n", qVecQueue.io.count)
  // printf("\n")

}
