package pe

import chisel3._
import chisel3.util._
import pe.utils._

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

  // L x D matrix for row dot row
  val kMatrix = IO(Input(Vec(L, Vec(D, UInt(bit.W)))))

  val InputPipe = IO(Flipped(Decoupled(new PipeValue(UInt(bit.W), D, numOfMask))))
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(bit.W), L, numOfMask)))

  val InputQueue = Module(
    new Queue(new PipeValue(UInt(bit.W), D, numOfMask), queueSize, pipe = true, flow = false, useSyncReadMem = false)
  )

  InputQueue.io.enq <> InputPipe

  val pes = for (i <- 0 until numOfMask) yield Module(new PE(bit, (i, 0), 0))
  for (i <- 0 until numOfMask) {
    pes(i).io := DontCare
  }
  val tempReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(bit.W))))
  // val tempK = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
  // tempK := kMatrix

//  val dataValid = WireInit(inMaskQueue.io.deq.valid && qVecQueue.io.deq.valid)
  val dataValid = WireInit(InputQueue.io.deq.valid)

  val tempQ = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))
  val tempMask = RegInit(VecInit(Seq.fill(numOfMask)(0.U(common.maskType.W))))

  val busy = RegInit(false.B)
  InputQueue.io.deq.ready := !busy

  val resValid = RegInit(false.B)
  OutputPipe.valid := resValid
  OutputPipe.bits := DontCare
  val finishedButNoAccepted = RegInit(false.B)

  val (cnt, warp) = Counter(0 until D, busy & (!resValid) & !(finishedButNoAccepted))

  when(busy) {
    when(!warp && !finishedButNoAccepted) {
      for (i <- 0 until numOfMask) {
        pes(i).io.inLeft := tempQ(cnt)
        pes(i).io.inTop := kMatrix(tempMask(i))(cnt)
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
        OutputPipe.bits.value(tempMask(i)) := tempReg(i)
      }
      OutputPipe.bits.mask := tempMask
      resValid := true.B
      when(resValid && OutputPipe.ready) {
        resValid := false.B
        finishedButNoAccepted := false.B
        busy := false.B
      }
    }

  }.otherwise {
    resValid := false.B
    when(dataValid) {
      tempQ := InputQueue.io.deq.bits.value
      tempMask := InputQueue.io.deq.bits.mask
      InputQueue.io.deq.ready := true.B
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
