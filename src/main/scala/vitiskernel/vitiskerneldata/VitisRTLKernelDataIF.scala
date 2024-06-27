package vitiskernel.vitiskerneldata

import chisel3._
import pe.utils.common
import vitiskernel.interface.{VitisAXIReadMaster, VitisAXIWriteMaster}

/**
  * Step1: Modify VitisRTLKernelDataIF
  */
class VitisRTLKernelDataIF extends Bundle {
  // Register Args
  val pipeReadLength = Input(UInt(64.W))
  val pipeReadAddress = Input(UInt(64.W))
  val pipeWriteAddress = Input(UInt(64.W))
  // add your register args here...
  val kReadLength = Input(UInt(64.W))
  val kReadAddress = Input(UInt(64.W))
  val kWriteAddress = Input(UInt(64.W))

  val vReadLength = Input(UInt(64.W))
  val vReadAddress = Input(UInt(64.W))
  val vWriteAddress = Input(UInt(64.W))

  // HBM/DDR ports
  val m00Read = new VitisAXIReadMaster(64, common.PIPE_DATA_WIDTH)
  val m00Write = new VitisAXIWriteMaster(64, common.PIPE_DATA_WIDTH)
  // add your memory ports here...

  val m01Read = new VitisAXIReadMaster(64, common.DATA_WIDTH)
  val m01Write = new VitisAXIWriteMaster(64, common.DATA_WIDTH)

  val m02Read = new VitisAXIReadMaster(64, common.DATA_WIDTH)
  val m02Write = new VitisAXIWriteMaster(64, common.DATA_WIDTH)
}
