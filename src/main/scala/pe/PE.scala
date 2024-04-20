package pe

import chisel3._
import chisel3.util._
import chisel3.experimental._


object ControlSignalSel extends ChiselEnum {
  val SDDMM, SPMM, SD_FINAL , SP_FINAL= Value
}

class PE(bit: Int, width: Int, id: (Int, Int), bufferSize: Int = 0, dim: Int) extends Module {
  val valueType = UInt(bit.W)

  val io = IO(new Bundle {
    val inTop = Input(valueType)
    val inLeft = Input(valueType)
    val controlSign = Input(ControlSignalSel())
    val inReg = Input(valueType)

    val outRight = Output(valueType)
    val outDown = Output(valueType)
    val outReg = Output(valueType)

  })

  io.outDown := DontCare
  io.outRight := DontCare
  io.outReg := DontCare

  switch(io.controlSign) {
    is(ControlSignalSel.SDDMM) {
      // MAC operation
      io.outReg := io.inReg + io.inTop * io.inLeft

      // shift left value to right like a Systolic array
      io.outRight := io.inLeft
    }
    is(ControlSignalSel.SPMM) {
      io.outReg := io.inReg + io.inTop * io.inLeft
    }
    is(ControlSignalSel.SD_FINAL) {
      // result of SDDMM will be stored at right
      io.outRight := io.inReg + io.inTop * io.inLeft
      io.outReg := 0.U
    }
    is(ControlSignalSel.SP_FINAL){
      // result of SPMM will be stored at down
      io.outDown := io.inReg + io.inTop * io.inLeft
      io.outReg := 0.U
    }
  }
}