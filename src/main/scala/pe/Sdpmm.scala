package pe

import chisel3._
import chisel3.util._

import chisel3.util.experimental.loadMemoryFromFileInline
import os.read.inputStream

class Sdpmm(val bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4, val queueSize: Int = 10)
    extends Module {

  val io = IO(new Bundle {
    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
  })

  val InputPipe = IO(Flipped(Decoupled(new PipeValue(UInt(bit.W), D, numOfMask))))
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(bit.W), D, numOfMask)))

  val sddmm = Module(new Sddmm(bit, D, L, numOfMask, queueSize))
  val SpMM = Module(new SpMM(bit, D, L, 1, numOfMask, queueSize))

  sddmm.kMatrix := io.kMatrix
  SpMM.vMatrix := io.vMatrix

  sddmm.InputPipe <> InputPipe

  // todo: softmax can be added here
  SpMM.InputPipe <> sddmm.OutputPipe

  OutputPipe <> SpMM.OutputPipe
}

class SdpmmOrigin(
  val bit:        Int = 16,
  val D:          Int = 32,
  val L:          Int = 32,
  val numOfMask:  Int = 4,
  val queueSize:  Int = 10,
  val FileName:   String = "",
  val inPutTimes: Int = 3)
    extends Module {

  val io = IO(new Bundle {
    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
  })

  val InputPipe = IO(Flipped(Decoupled(new PipeValue(UInt(bit.W), D, numOfMask))))
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(bit.W), D, numOfMask)))

  val sddmm = Module(new Sddmm(bit, D, L, numOfMask, queueSize))
  val SpMM = Module(new SpMM(bit, D, L, 1, numOfMask, queueSize))

  sddmm.kMatrix := io.kMatrix
  SpMM.vMatrix := io.vMatrix

  sddmm.InputPipe <> InputPipe

  val memMiddle = Module(
    new ForwardingDelayMemory(
      new PipeValue(UInt(bit.W), L, numOfMask),
      L * bit + numOfMask * utils.maskType,
      inPutTimes
    )
  )
  sddmm.OutputPipe <> memMiddle.io.wrData
  SpMM.InputPipe <> memMiddle.io.rdData

  OutputPipe <> SpMM.OutputPipe
}
