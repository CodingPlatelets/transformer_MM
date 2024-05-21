package pe

import chisel3._
import chisel3.util._
import chisel3.experimental._

object utils {
  def counter(max: UInt, cond: Bool) = {
    val x = RegInit(0.U(max.getWidth.W))
    when(cond) {
      x := Mux(x === max, 0.U, x + 1.U)
    }
    x
  }

  def counter(max: Int, initCond: Bool) = {
    val x = RegInit(0.U(log2Ceil(max).W))
    when(initCond && x === 0.U) {
      x := x + 1.U
    }.elsewhen(x =/= 0.U) {
      x := Mux(x === max.U, 0.U, x + 1.U)
    }
    x
  }

  // this will find the last "one" in an UInt, and then convert it to a one hot num
  def maskOH(mask: UInt) = {
    mask - (mask & (mask - 1.U))
  }

  val maskType = 16
}
class PipelineProducer extends Module {
  val io = IO(new Bundle {
    val a = Flipped(DecoupledIO(UInt(8.W)))
    val b = DecoupledIO(UInt(8.W))
  })

  val (cnt, cntT) = Counter(true.B, 1)
  val result = Reg(UInt(8.W))

  io.a.ready := RegInit(io.a.valid) & cnt === 0.U
  result := io.a.bits + 1.U

  io.b.valid := RegInit(io.b.ready) & cntT
  io.b.bits := result

}

class PipelineConsumer extends Module {
  val io = IO(new Bundle {
    val a = Flipped(DecoupledIO(UInt(8.W)))
    val b = DecoupledIO(UInt(8.W))
  })

  val (cnt, cntT) = Counter(true.B, 2)
  val result = Reg(UInt(8.W))

  io.a.ready := RegInit(io.a.valid) & cnt === 0.U
  result := io.a.bits + 1.U

  io.b.valid := RegInit(io.b.ready) & cntT
  io.b.bits := result
}

class ValidReady extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(UInt(8.W))
  })

  val p = Module(new PipelineProducer)
  val c = Module(new PipelineConsumer)

  p.io.a <> io.in
  p.io.b <> c.io.a
  c.io.b <> io.out

}

object ControlSignalSel extends ChiselEnum {
  val SDDMM, SPMM, SD_FINAL, SP_FINAL = Value
}

class PE(bit: Int, id: (Int, Int), bufferSize: Int = 0) extends Module {
  val valueType = UInt(bit.W)
  val resType = UInt(bit.W)

  val io = IO(new Bundle {
    val inTop = Input(valueType)
    val inLeft = Input(valueType)
    val controlSign = Input(ControlSignalSel())
    val inReg = Input(valueType)

    val outRight = Output(valueType)
    val outReg = Output(resType)

  })

  io.outRight := DontCare
  io.outReg := DontCare

  switch(io.controlSign) {
    is(ControlSignalSel.SDDMM) {

      // MAC operation
      io.outReg := io.inReg +& (io.inTop * io.inLeft)

      // shift left value to right like a Systolic array
      // TODO delete the design
      io.outRight := RegNext(io.inLeft)
    }
    is(ControlSignalSel.SPMM) {
      io.outReg := io.inReg +& (io.inTop * io.inLeft)
    }
  }
}
