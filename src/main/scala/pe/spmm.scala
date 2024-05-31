package pe

import chisel3._
import chisel3.util._

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

  val numsQueue = Module(new Queue(Vec(L, UInt(bit.W)), queueSize, pipe = true, flow = false,useSyncReadMem = false ))
  val inMaskQueue = Module(new Queue(Vec(numOfMask, UInt(utils.maskType.W)), queueSize, pipe = true, flow = false, useSyncReadMem = false))

  val tempRegVec = RegInit(VecInit(Seq.fill(dimV)(0.U(bit.W))))
  // val pes = VecInit(Seq.fill(dimV)(Module(new PE(bit, (0, 0), 0)).io))
  val pes = for (i <- 0 until dimV) yield Module(new PE(bit, (0, 0), 0))
  for (i <- 0 until dimV) {
    pes(i).io := DontCare
  }

  numsQueue.io.deq := DontCare
  inMaskQueue.io.deq := DontCare
  io.res.bits := DontCare
  io.outMask.bits := DontCare

  io.inMask <> inMaskQueue.io.enq
  io.nums <> numsQueue.io.enq

  val maskReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(utils.maskType.W))))
  val numsReg = RegInit(VecInit(Seq.fill(L)(0.U(bit.W))))
  val vMatrixReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(dimV)(0.U(bit.W))))))
  vMatrixReg := io.vMatrix

  val busy = RegInit(false.B)
  inMaskQueue.io.deq.ready := !busy
  numsQueue.io.deq.ready := !busy

  val resValid = RegInit(false.B)
  io.res.valid := resValid
  io.outMask.valid := resValid
  val finishedButNoAccepted = RegInit(false.B)

  val dataValid = WireInit(inMaskQueue.io.deq.valid && numsQueue.io.deq.valid)

  val (cnt, warp) = Counter(0 until numOfMask, busy & (!resValid) & (!finishedButNoAccepted))
  // printf("current cnt is %d\n", cnt)
  // printf("current tempRegVec(0) is %d\n", tempRegVec(0))
  // printf("current pes.io.outReg is %d\n", pes(0).io.outReg)
  // printf("current pes.io.inTop is %d\n", pes(0).io.inTop)
  // printf("current pes.io.inLeft is %d\n", pes(0).io.inLeft)
  // printf("current pes.io.inReg is %d\n", pes(0).io.inReg)
  // printf("current inQueue Count is %d\n", numsQueue.io.count)
  // printf("\n")

  when(busy) {
    when(!warp && !finishedButNoAccepted) {
      for (i <- 0 until dimV) {
        pes(i).io.controlSign := ControlSignalSel.SPMM
        pes(i).io.inTop := io.vMatrix(maskReg(cnt))(i)
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
        io.res.bits(i) := tempRegVec(i)
      }
      io.outMask.bits := maskReg
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
      maskReg := inMaskQueue.io.deq.bits
      numsReg := numsQueue.io.deq.bits
      inMaskQueue.io.deq.ready := true.B
      numsQueue.io.deq.ready := true.B
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
