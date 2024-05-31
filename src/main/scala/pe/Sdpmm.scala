package pe

import chisel3._
import chisel3.util._

import chisel3.util.experimental.loadMemoryFromFileInline
import os.read.inputStream

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

  // pipMask.io.enq := sddmm.io.outMask
  // pipMiddle.io.enq := sddmm.io.res
  // SpMM.io.nums := pipMiddle.io.deq
  // SpMM.io.inMask := pipMask.io.deq
//  memMask.io := DontCare
//  memMiddle.io := DontCare
//
//  val memAddr = RegInit(VecInit(Seq.tabulate(inPutTimes)(i => i.U(12.W))))
//  val sdValid = WireInit(sddmm.io.outMask.valid && sddmm.io.res.valid)
//
//  val stored = RegInit(false.B)
//  val currentLeft = Counter(
//    0 to inPutTimes,
//    !stored && sdValid
//  )
//
//  when(sdValid && !stored) {
//    memMask.io.wrEna := true.B
//    memMiddle.io.wrEna := true.B
//    memMiddle.io.wrAddr := memAddr(currentLeft._1)
//    memMask.io.wrAddr := memAddr(currentLeft._1)
//    // printf("mem write address is %d\n", memAddr(currentLeft._1))
//    memMask.io.wrData := sddmm.io.outMask.bits
//    memMiddle.io.wrData := sddmm.io.res.bits
//
//    sddmm.io.res.ready := true.B
//    sddmm.io.outMask.ready := true.B
//
//    // printf("mem write data(0) is %d\n", sddmm.io.res.bits(sddmm.io.outMask.bits(0)))
//    // for (i <- 0 until numOfMask) {
//    //   printf("wdata(%d) is %d\t", i.U, sddmm.io.res.bits(sddmm.io.outMask.bits(i)))
//    // }
//    // printf("\n")
//
//  }.otherwise {
//    memMask.io.wrEna := false.B
//    memMiddle.io.wrEna := false.B
//
//    sddmm.io.res.ready := false.B
//    sddmm.io.outMask.ready := false.B
//  }
//
//  when(currentLeft._1 === inPutTimes.U) {
//    stored := true.B
//  }
//
//  val send = RegInit(false.B)
//  when(stored) {
//    send := true.B
//  }
//  val readFinished = RegInit(false.B)
//
//  val currentRight = Counter(0 to inPutTimes, send && !readFinished)
//
//  when(currentRight._1 === inPutTimes.U) {
//    send := false.B
//  }
//
//  val resValid = RegInit(false.B)
//
//  when(send && !readFinished) {
//    memMask.io.rdAddr := memAddr(currentRight._1)
//    memMiddle.io.rdAddr := memAddr(currentRight._1)
//
//    // printf("mem read address is %d\n", memAddr(currentRight._1))
//    readFinished := true.B
//    SpMM.io.inMask.valid := false.B
//    SpMM.io.nums.valid := false.B
//  }.elsewhen(send && readFinished) {
//    SpMM.io.inMask.bits := memMask.io.rdData
//    SpMM.io.nums.bits := memMiddle.io.rdData
//
//    // for (i <- 0 until numOfMask) {
//    //   printf("rdata(%d) is %d\t", i.U, SpMM.io.nums.bits(SpMM.io.inMask.bits(i)))
//    // }
//    // printf("\n")
//
//    SpMM.io.inMask.valid := true.B
//    SpMM.io.nums.valid := true.B
//    readFinished := false.B
//  }.otherwise {
//    SpMM.io.inMask.valid := false.B
//    SpMM.io.nums.valid := false.B
//  }

  // val spReady = Wire(Bool())
  // val spReady = RegNext(SpMM.io.inMask.ready && SpMM.io.nums.ready && isStored && !sdValid && currentLeft._1 === 0.U)
  // when(currentLeft._2) {
  //   isStored := true.B
  // }
  // val readFinished = RegInit(false.B)
  // val isSend = RegInit(false.B)

  // val currentRight = Counter(0 until inPutTimes, spReady && !readFinished)

  // when(currentRight._2) {
  //   isSend := true.B
  // }
  // when(spReady && !readFinished && !isSend) {
  //   memMask.io.rdAddr := memAddr(currentRight._1)
  //   printf("mem read address is %d\n", memAddr(currentRight._1))

  //   memMiddle.io.rdAddr := memAddr(currentRight._1)
  //   readFinished := true.B
  // }.elsewhen(spReady && readFinished && !isSend) {
  //   SpMM.io.inMask.valid := true.B
  //   SpMM.io.nums.valid := true.B
  //   SpMM.io.inMask.bits := memMask.io.rdData
  //   SpMM.io.nums.bits := memMiddle.io.rdData
  //   printf("mem read num(mask(0)) is %d\n", SpMM.io.nums.bits(SpMM.io.inMask.bits(0)))
  //   readFinished := false.B
  // }.otherwise {
  //   SpMM.io.inMask.valid := false.B
  //   SpMM.io.nums.valid := false.B
  // }

  io.res <> SpMM.io.res
  io.outMask <> SpMM.io.outMask

}
