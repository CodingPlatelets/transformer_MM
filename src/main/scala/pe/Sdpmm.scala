package pe

import chisel3._
import chisel3.util._

class Sdpmm(val bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4, val queueSize: Int = 10)
    extends Module {

  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val nums = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(D, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))

    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
  })

  val sddmm = Module(new Sddmm(bit, D, L, numOfMask, queueSize))
  val SpMM = Module(new SpMM(bit, D, L, 1, numOfMask, queueSize))

  sddmm.io.kMatrix := io.kMatrix
  SpMM.io.vMatrix := io.vMatrix

  sddmm.io.inMask <> io.inMask
  sddmm.io.qVec <> io.nums

  // todo: softmax can be added here
  SpMM.io.inMask <> sddmm.io.outMask
  SpMM.io.nums <> sddmm.io.res

  io.res <> SpMM.io.res
  io.outMask <> SpMM.io.outMask

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
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val nums = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(D, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))

    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
  })

  val sddmm = Module(new Sddmm(bit, D, L, numOfMask, queueSize))
  val SpMM = Module(new SpMM(bit, D, L, 1, numOfMask, queueSize))
  sddmm.io.res.ready := DontCare
  sddmm.io.outMask.ready := DontCare
  SpMM.io.inMask := DontCare
  SpMM.io.nums := DontCare

  sddmm.io.kMatrix := io.kMatrix
  SpMM.io.vMatrix := io.vMatrix

  sddmm.io.inMask <> io.inMask
  sddmm.io.qVec <> io.nums

  // val memMask = Module(
  //   new QueueModule(Vec(numOfMask, UInt(utils.maskType.W)), inPutTimes, useMem = true, pipe = false, flow = false)
  // )
  // val memMiddle = Module(
  //   new QueueModule(Vec(L, UInt(bit.W)), inPutTimes, useMem = true, pipe = false, flow = false)
  // )
  // memMask.in <> sddmm.io.outMask
  // memMiddle.in <> sddmm.io.res
  // memMiddle.out <> SpMM.io.nums
  // memMask.out <> SpMM.io.inMask

  val pipMask = Pipe(sddmm.io.outMask.valid, sddmm.io.outMask.bits, inPutTimes)
  val pipMiddle = Pipe(sddmm.io.res.valid, sddmm.io.res.bits, inPutTimes)

  val memMask = Module(
    new QueueModule(Vec(numOfMask, UInt(utils.maskType.W)), 1, useMem = true, pipe = false, flow = false)
  )
  val memMiddle = Module(
    new QueueModule(Vec(L, UInt(bit.W)), 1, useMem = true, pipe = false, flow = false)
  )

  sddmm.io.outMask.ready := memMask.in.ready
  sddmm.io.res.ready := memMiddle.in.ready

  memMask.in.valid := pipMask.valid
  memMask.in.bits := pipMask.bits

  memMiddle.in.valid := pipMiddle.valid
  memMiddle.in.bits := pipMiddle.bits

  memMiddle.out <> SpMM.io.nums
  memMask.out <> SpMM.io.inMask

  io.res <> SpMM.io.res
  io.outMask <> SpMM.io.outMask

}
