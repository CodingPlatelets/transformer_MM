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
    val idle, calculate = Value
  }

  val state = RegInit(State.idle)

  val cnt = utils.counter(D, state === State.calculate && io.colK.valid && io.rowQ.valid)

  io.rowQ.ready := Mux(cnt === 0.U, true.B, false.B)
  io.colK.ready := Mux(cnt === 0.U, true.B, false.B)
  when(io.colK.valid && io.rowQ.valid && io.colK.ready && io.rowQ.ready) {
    temQ := io.rowQ.bits
    temK := io.colK.bits
  }

  when(io.res.valid && io.res.ready) {
    io.res.valid := false.B
  }

  switch(state) {
    is(State.idle) {
      io.res.valid := false.B
      when(cnt === 0.U) {
        state := State.calculate
        io.res.valid := false.B
      }
    }

    is(State.calculate) {
      when(cnt === D.U) {
        state := State.calculate
        io.res.valid := true.B
        io.res.bits := tempReg
      }

      when((cnt > 0.U && cnt < D.U) || (io.colK.valid && io.rowQ.valid && cnt === 0.U)) {
        io.res.valid := false.B
        pe.io.controlSign := ControlSignalSel.SDDMM
        pe.io.inLeft := Mux(cnt === 0.U, io.rowQ.bits(cnt), temQ(cnt))
        pe.io.inTop := Mux(cnt === 0.U, io.colK.bits(cnt), temK(cnt))
        pe.io.inReg := Mux(cnt === 0.U, 0.U, tempReg)
        tempReg := pe.io.outReg
        // printf("pe(%d) in cycle %d, temReg is %d\n", index.U, cnt, tempReg)
      }
    }
  }
}

// todo: cannot create pes dynamically, so we must fix the numOfMask, but we can set a min numOfMask and schedule it in the future
class sddmm(bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4) extends Module {
  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val qVec = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    // L x D matrix for row dot row
    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(L, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))
  })

  io := DontCare
  io.res.bits := VecInit(Seq.fill(L)(0.U(bit.W)))
  io.res.valid := false.B
  io.outMask.valid := false.B

  val temKReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
  temKReg := io.kMatrix

  val vecPe = for {
    i <- 0 until numOfMask
  } yield Module(new VecDotVec(i, bit, D))

  for (i <- 0 until numOfMask) {
    vecPe(i).io := DontCare
  }

  // subModule ready or not
  val vecPeValid = VecInit(vecPe.map(_.io.res.valid)).reduceTree(_ && _)
  val vecPeQReady = VecInit(vecPe.map(_.io.rowQ.ready)).reduceTree(_ && _)
  val vecPeKReady = VecInit(vecPe.map(_.io.colK.ready)).reduceTree(_ && _)
  val tempQReg = RegInit(VecInit(Seq.fill(D)(0.U(bit.W))))

  val tempMaskReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(utils.maskType.W))))

  object State extends ChiselEnum {
    val idle, sendData, result = Value
  }

  val state = RegInit(State.idle)
  val tempResReg = RegInit(VecInit(Seq.fill(numOfMask)(0.U(bit.W))))

  when(io.inMask.valid && io.qVec.valid) {
    tempMaskReg := io.inMask.bits
    tempQReg := io.qVec.bits
  }

  // the bits will only send in one cycle
  when(io.res.ready && io.outMask.ready && io.res.valid && io.outMask.valid) {
    io.res.valid := false.B
    io.outMask.valid := false.B
  }

  io.inMask.ready := !io.outMask.valid
  io.qVec.ready := !io.res.valid

  switch(state) {
    is(State.idle) {
      when(io.inMask.valid && io.qVec.valid) {
        state := State.sendData
        for (i <- 0 until numOfMask) {
          vecPe(i).io.rowQ.valid := true.B
          vecPe(i).io.colK.valid := true.B
          vecPe(i).io.rowQ.bits := io.qVec.bits
          vecPe(i).io.colK.bits := temKReg(tempMaskReg(i))
        }
      }
      for (i <- 0 until numOfMask) {
        vecPe(i).io.rowQ.valid := false.B
        vecPe(i).io.colK.valid := false.B
      }

    }
    is(State.sendData) {
      when(vecPeValid) {
        io.res.valid := true.B
        // printf("temResReg(0) is %d \n", tempResReg(0))
        for (i <- 0 until numOfMask) {
          io.res.bits(tempMaskReg(i)) := Mux(vecPe(i).io.res.bits === 0.U, tempResReg(i), vecPe(i).io.res.bits)
          tempResReg(i) := Mux(vecPe(i).io.res.bits === 0.U, tempResReg(i), vecPe(i).io.res.bits)
        }

        io.outMask.valid := true.B
        io.outMask.bits := tempMaskReg

      }

      when(vecPeQReady && vecPeKReady && io.inMask.ready && io.qVec.ready) {
        io.res.valid := false.B
        io.outMask.valid := false.B
        printf("cal is begin\n")
        printf("tempMaskReg(0) is %d\n", tempMaskReg(0))
        for (i <- 0 until numOfMask) {
          vecPe(i).io.rowQ.valid := true.B
          vecPe(i).io.colK.valid := true.B
          vecPe(i).io.rowQ.bits := io.qVec.bits
          vecPe(i).io.colK.bits := temKReg(tempMaskReg(i))
        }
      }

    }
  }
}
