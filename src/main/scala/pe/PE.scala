package pe

import chisel3._
import chisel3.util._

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
