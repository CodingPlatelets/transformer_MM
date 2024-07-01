package pe

import chisel3._
import chisel3.util._
import vitiskernel.mmstream._
import vitiskernel.vitiskerneldata.VitisRTLKernelDataIF
import utils.PipeValue
import utils.common
import pe.configs.SdpmmConfigs

class TOPSdpmm extends Module {
  val io = IO(new Bundle {
    val dataIF = (new VitisRTLKernelDataIF)
    val done = Output(Bool())
  })

  val sdpmmModule = Module(
    new Sdpmm(
      SdpmmConfigs.bit,
      SdpmmConfigs.dim,
      SdpmmConfigs.L,
      SdpmmConfigs.numOfMask,
      SdpmmConfigs.queueSize
    )
  )

  //////////////////////////  KMatrix  ////////////////////////
  val kReadReqIssuedReg = RegInit(false.B)
  val kWriteReqIssuedReg = RegInit(false.B)
  val mm2s_k = Module(new MM2S(64, common.DATA_WIDTH))
  val s2mm_k = Module(new S2MM(64, common.DATA_WIDTH))
  mm2s_k.io.axiRead <> io.dataIF.m01Read
  s2mm_k.io.axiWrite <> io.dataIF.m01Write
  mm2s_k.io.req.bits.addr := io.dataIF.kReadAddress
  mm2s_k.io.req.bits.len := io.dataIF.kReadLength
  s2mm_k.io.req.bits.addr := io.dataIF.kWriteAddress

  mm2s_k.io.req.valid := !kReadReqIssuedReg
  s2mm_k.io.req.valid := !kWriteReqIssuedReg
  when(mm2s_k.io.req.ready) {
    kReadReqIssuedReg := true.B
  }
  when(s2mm_k.io.req.ready) {
    kWriteReqIssuedReg := true.B
  }
  val kLine = Wire(UInt(common.DATA_WIDTH.W))

  // size: 2MB
  val kIntReg = RegInit(0.U((32 * common.DATA_WIDTH).W))

  mm2s_k.io.streamOut.ready := s2mm_k.io.streamIn.ready
  s2mm_k.io.streamIn.valid := mm2s_k.io.streamOut.valid
  kLine := mm2s_k.io.streamOut.bits.data.asTypeOf(chiselTypeOf(kLine))

  val kLineOut = Wire(Vec(32, UInt(16.W)))
  kLineOut
    .zip(kLine.asTypeOf(kLineOut))
    .foreach(p => {
      p._1 := p._2 + 10.U
    })

  when(mm2s_k.io.streamOut.valid && s2mm_k.io.streamIn.ready) {
    kIntReg := kLine + (kIntReg << common.DATA_WIDTH)
  }
  s2mm_k.io.streamIn.bits.data := kLineOut.asUInt
  s2mm_k.io.streamIn.bits.last := mm2s_k.io.streamOut.bits.last

  val kFinished = RegInit(false.B)
  val (_, kWarp) = Counter(0 until 32, mm2s_k.io.streamOut.valid && s2mm_k.io.streamIn.ready)
  kFinished := Mux(kWarp, true.B, false.B)
  sdpmmModule.io.kMatrix := kIntReg.asTypeOf(
    chiselTypeOf(Reg(Vec(SdpmmConfigs.L, Vec(SdpmmConfigs.dim, UInt(16.W)))))
  )

  //////////////////////////  VMatrix  ////////////////////
  val vReadReqIssuedReg = RegInit(false.B)
  val vWriteReqIssuedReg = RegInit(false.B)
  val mm2s_v = Module(new MM2S(64, common.DATA_WIDTH))
  val s2mm_v = Module(new S2MM(64, common.DATA_WIDTH))
  mm2s_v.io.axiRead <> io.dataIF.m02Read
  s2mm_v.io.axiWrite <> io.dataIF.m02Write
  mm2s_v.io.req.bits.addr := io.dataIF.vReadAddress
  mm2s_v.io.req.bits.len := io.dataIF.vReadLength
  s2mm_v.io.req.bits.addr := io.dataIF.vWriteAddress

  mm2s_v.io.req.valid := !kReadReqIssuedReg
  s2mm_v.io.req.valid := !kWriteReqIssuedReg
  when(mm2s_v.io.req.ready) {
    kReadReqIssuedReg := true.B
  }
  when(s2mm_v.io.req.ready) {
    kWriteReqIssuedReg := true.B
  }
  val vLine = Wire(UInt(common.DATA_WIDTH.W))
  // size: 2MB
  val vIntReg = RegInit(0.U((32 * common.DATA_WIDTH).W))

  mm2s_v.io.streamOut.ready := s2mm_v.io.streamIn.ready
  s2mm_v.io.streamIn.valid := mm2s_v.io.streamOut.valid

  vLine := mm2s_v.io.streamOut.bits.data.asTypeOf(chiselTypeOf(vLine))
  val vLineOut = Wire(Vec(32, UInt(16.W)))
  vLineOut
    .zip(vLine.asTypeOf(vLineOut))
    .foreach(p => {
      p._1 := p._2 + 20.U
    })

  when(mm2s_v.io.streamOut.valid && s2mm_v.io.streamIn.ready) {
    vIntReg := vLine + (vIntReg << common.DATA_WIDTH)
  }
  s2mm_v.io.streamIn.bits.data := vLineOut.asUInt
  s2mm_v.io.streamIn.bits.last := mm2s_v.io.streamOut.bits.last

  val vFinished = RegInit(false.B)
  val (_, vWarp) = Counter(0 until 32, mm2s_v.io.streamOut.valid && s2mm_v.io.streamIn.ready)
  vFinished := Mux(vWarp, true.B, false.B)
  sdpmmModule.io.vMatrix := vIntReg.asTypeOf(
    chiselTypeOf(Reg(Vec(SdpmmConfigs.L, Vec(SdpmmConfigs.dim, UInt(16.W)))))
  )

  ////////////////////////  Pipe Data Flow  ////////////////////////
  // 在 reset 直接开始执行，执行结束后将 done 置位即可
  val readReqIssuedReg = RegInit(false.B)
  val writeReqIssuedReg = RegInit(false.B)

  val mm2s_pipe = Module(new MM2S(64, common.PIPE_DATA_WIDTH))
  val s2mm_pipe = Module(new S2MM(64, common.PIPE_DATA_WIDTH))

  // dataIF connection
  mm2s_pipe.io.axiRead <> io.dataIF.m00Read
  s2mm_pipe.io.axiWrite <> io.dataIF.m00Write
  mm2s_pipe.io.req.bits.addr := io.dataIF.pipeReadAddress
  mm2s_pipe.io.req.bits.len := io.dataIF.pipeReadLength
  s2mm_pipe.io.req.bits.addr := io.dataIF.pipeWriteAddress

  mm2s_pipe.io.req.valid := !readReqIssuedReg
  s2mm_pipe.io.req.valid := !writeReqIssuedReg
  when(mm2s_pipe.io.req.ready) {
    readReqIssuedReg := true.B
  }
  when(s2mm_pipe.io.req.ready) {
    writeReqIssuedReg := true.B
  }

  // s2mm_pipe.io.streamIn.valid := mm2s_pipe.io.streamOut.valid
  // mm2s_pipe.io.streamOut.ready := s2mm_pipe.io.streamIn.ready

  // 32 * 16 + 32 * maskType(16) = 1024
  val inPipeData = Wire(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask))

  // data prepare
  inPipeData := mm2s_pipe.io.streamOut.bits.data.asTypeOf(chiselTypeOf(inPipeData))
  sdpmmModule.InputPipe.bits := inPipeData
  sdpmmModule.InputPipe.valid := mm2s_pipe.io.streamOut.valid && kFinished && vFinished
  mm2s_pipe.io.streamOut.ready := sdpmmModule.InputPipe.ready
  mm2s_pipe.io.streamOut.bits.last := DontCare

  sdpmmModule.OutputPipe.ready := s2mm_pipe.io.streamIn.ready
  s2mm_pipe.io.streamIn.valid := sdpmmModule.OutputPipe.valid
  s2mm_pipe.io.streamIn.bits.data := sdpmmModule.OutputPipe.bits.asUInt
  s2mm_pipe.io.streamIn.bits.last := sdpmmModule.OutputPipe.bits.asUInt.asBools.last

  io.done := readReqIssuedReg && writeReqIssuedReg && !mm2s_pipe.io.busy && !s2mm_pipe.io.busy &&
    kReadReqIssuedReg && kWriteReqIssuedReg && !mm2s_k.io.busy && !s2mm_k.io.busy && vReadReqIssuedReg &&
    vWriteReqIssuedReg && !mm2s_v.io.busy && !s2mm_v.io.busy
}
