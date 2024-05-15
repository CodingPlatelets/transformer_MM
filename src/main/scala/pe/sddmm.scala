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

class VecDotVec(val bit: Int, D: Int = 32) extends Module {
  val io = IO(new Bundle {
    val rowQ = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val colK = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val res = Decoupled(UInt(bit.W))
  })
  // using D cycle to calculate the result
  val tempReg = RegInit(0.U(bit.W))
  val pe = Module(new PE(bit, (0, 0), 0))
  pe.io := DontCare
  io.rowQ.ready := DontCare
  io.colK.ready := DontCare
  io.res := DontCare

  object State extends ChiselEnum {
    val idle, calculate = Value
  }

  val state = RegInit(State.idle)

  val (counterValue, counterWrap) = Counter(state === State.calculate && io.colK.valid && io.rowQ.valid, D + 1)
  switch(state) {
    is(State.idle) {
      printf("State is idle\n")
      io.res.valid := false.B
      when(counterValue === 0.U) {
        state := State.calculate
      }
    }

    is(State.calculate) {
      printf("State is calculate\n")
      printf("counter is %d\n", counterValue)
      printf("left is %d , top is %d, temreg is %d\n", pe.io.inLeft, pe.io.inTop, tempReg)
      when(counterValue === 0.U) {
        io.rowQ.ready := true.B
        io.colK.ready := true.B
        io.res.valid := false.B
      }
      when(counterValue === D.U) {
        state := State.calculate
        io.colK.ready := false.B
        io.rowQ.ready := false.B
        io.res.valid := true.B
        io.res.bits := tempReg
      }
      when(counterValue < D.U && io.colK.valid && io.rowQ.valid) {
        pe.io.controlSign := ControlSignalSel.SDDMM
        pe.io.inLeft := io.rowQ.bits(counterValue)
        pe.io.inTop := io.colK.bits(counterValue)
        pe.io.inReg := Mux(counterValue === 0.U, 0.U, tempReg)
        tempReg := pe.io.outReg
      }
    }
  }
}

// todo: cannot create pes dynamically, so we must fix the numOfMask, but we can set a min numOfMask and schedule it in the future
class sddmm(bit: Int = 8, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4) extends Module {
  val io = IO(new Bundle {
    val mask = Input(Vec(L, Vec(L, Bool())))
    val qVec = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val qIndex = Input(UInt(D.W))
    val kMatrix = Input(Vec(D, Vec(L, UInt(bit.W))))
  })

  // using L PEs and L mux one hot to choose and calculate the result
  val pes = for {
    i <- 0 until L
    j <- 0 until numOfMask
  } yield Module(new PE(bit, (i, j), 0))

  val oneHotReg = RegInit(VecInit(Seq.fill(L)(0.U(L.W))))

  val tempPeRegVec = RegInit(VecInit(Seq.fill(L)(0.U(bit.W))))

  for (i <- 0 until numOfMask) {
    pes(i).io := DontCare
  }

  val tempMaskReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(L)(false.B)))))

  object State extends ChiselEnum {
    val idle, calculate, result = Value
  }

  val state = RegInit(State.idle)
  val (counterValue, counterWrap) = Counter(io.qVec.valid && io.qVec.ready, D)

  switch(state) {
    is(State.idle) {
      tempMaskReg := io.mask
    }
  }
}
