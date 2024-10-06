package pe

import chisel3._
import chisel3.util._
import pe.utils._

// spmm using NumDotVec via stream data input
// using mask to choose the needed nums
// to find one's position, using n - n & (n-1) and than one hot to int
// a row of a L x L matrix with mask select the needed nums will dot the specific row of the V matrix selected by the same mask
class SpMM(bit: Int = 8, dimV: Int = 32, val L: Int = 32, alu: Int = 1, val numOfMask: Int, val queueSize: Int = 10)
    extends Module
    with DebugLog {

  val vMatrix = IO(Input(Vec(L, Vec(dimV, UInt(bit.W)))))

  val InputPipe = IO(Flipped(Decoupled(new PipeValue(UInt(bit.W), L, numOfMask))))
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(bit.W), dimV, numOfMask)))

  val InputQueue = Module(
    new Queue(new PipeValue(UInt(bit.W), L, numOfMask), queueSize, pipe = false, flow = false, useSyncReadMem = false)
  )

  InputQueue.io.enq <> InputPipe

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U(bit.W))))
  // val pes = VecInit(Seq.fill(dimV)(Module(new PE(bit, (0, 0), 0)).io))
  val pes = for (i <- 0 until dimV) yield Module(new PE(bit, (0, 0), 0))
  for (i <- 0 until dimV) {
    pes(i).io := DontCare
  }

  InputQueue.io.deq := DontCare
  InputQueue.io.deq := DontCare
  OutputPipe.bits := DontCare
  OutputPipe.bits := DontCare

  val maskReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(common.maskType.W))))
  val numsReg = RegInit(VecInit(Seq.fill(L)(0.U(bit.W))))
  // val vMatrixReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(dimV)(0.U(bit.W))))))
  // vMatrixReg := vMatrix

  val busy = RegInit(false.B)
  InputQueue.io.deq.ready := !busy

  val resValid = RegInit(false.B)
  OutputPipe.valid := resValid
  val finishedButNoAccepted = RegInit(false.B)

  val dataValid = WireDefault(InputQueue.io.deq.valid)

  val (cnt, warp) = Counter(0 until numOfMask, busy & (!resValid) & (!finishedButNoAccepted))

  debugLog(
    p"Spmm:\n" +
      p"current cnt is ${cnt}" +
      p"current tempRegVec(0) is ${tempRegVec(0)} " +
      p"current pes.io.outReg is ${pes(0).io.outReg} " +
      p"current pes.io.inTop is  ${pes(0).io.inTop} " +
      p"current pes.io.inLeft is ${pes(0).io.inLeft} " +
      p"current pes.io.inReg is  ${pes(0).io.inReg} " +
      p"current inQueue Count is ${InputQueue.io.count}\n"
  )

  when(busy) {
    when(!warp && !finishedButNoAccepted) {
      for (i <- 0 until dimV) {
        pes(i).io.controlSign := ControlSignalSel.SPMM
        pes(i).io.inTop := vMatrix(maskReg(cnt))(i)
        pes(i).io.inLeft := numsReg(maskReg(cnt))
        pes(i).io.inReg := Mux(cnt === 0.U, 0.U, tempRegVec(i))
        tempRegVec(i) := pes(i).io.outReg
      }
    }

    when(warp) {
      for (i <- 0 until dimV) {
        tempRegVec(i) := pes(i).io.outReg
      }
      finishedButNoAccepted := true.B
    }

    when(finishedButNoAccepted) {
      for (i <- 0 until dimV) {
        OutputPipe.bits.value(i) := tempRegVec(i)
      }
      OutputPipe.bits.mask := maskReg
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
      maskReg := InputQueue.io.deq.bits.mask
      numsReg := InputQueue.io.deq.bits.value
      InputQueue.io.deq.ready := true.B
      busy := true.B
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
