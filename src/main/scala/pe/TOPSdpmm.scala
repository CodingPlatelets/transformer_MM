package pe

import chisel3._
import chisel3.util._
import vitiskernel.mmstream._
import vitiskernel.vitiskerneldata.VitisRTLKernelDataIF
import utils.PipeValue
import utils.common
import utils.DebugLog
import pe.configs.SdpmmConfigs

class TOPSdpmm extends Module with DebugLog {
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
  val kLine = RegInit(0.U(common.DATA_WIDTH.W))

  // size: 2MB
  // val kIntReg = RegInit(0.U((SdpmmConfigs.L * common.DATA_WIDTH).W))
  val kIntReg = RegInit(VecInit(Seq.fill(SdpmmConfigs.L)(VecInit(Seq.fill(SdpmmConfigs.dim)(0.U(16.W))))))

  kLine := mm2s_k.io.streamOut.bits.data

  sdpmmModule.io.kMatrix := kIntReg

  //////////////////////////  VMatrix  ///////////////////////////
  val vReadReqIssuedReg = RegInit(false.B)
  val vWriteReqIssuedReg = RegInit(false.B)
  val mm2s_v = Module(new MM2S(64, common.DATA_WIDTH))
  val s2mm_v = Module(new S2MM(64, common.DATA_WIDTH))
  mm2s_v.io.axiRead <> io.dataIF.m02Read
  s2mm_v.io.axiWrite <> io.dataIF.m02Write
  mm2s_v.io.req.bits.addr := io.dataIF.vReadAddress
  mm2s_v.io.req.bits.len := io.dataIF.vReadLength
  s2mm_v.io.req.bits.addr := io.dataIF.vWriteAddress

  mm2s_v.io.req.valid := !vReadReqIssuedReg
  s2mm_v.io.req.valid := !vWriteReqIssuedReg
  when(mm2s_v.io.req.ready) {
    vReadReqIssuedReg := true.B
  }
  when(s2mm_v.io.req.ready) {
    vWriteReqIssuedReg := true.B
  }
  val vLine = RegInit(0.U(common.DATA_WIDTH.W))
  // size: 2MB
  // val vIntReg = RegInit(0.U((SdpmmConfigs.L * common.DATA_WIDTH).W))
  val vIntReg = RegInit(VecInit(Seq.fill(SdpmmConfigs.L)(VecInit(Seq.fill(SdpmmConfigs.dim)(0.U(16.W))))))

  vLine := mm2s_v.io.streamOut.bits.data

  sdpmmModule.io.vMatrix := vIntReg

  object state extends ChiselEnum {
    val kvIdle, kvDataRead, kvDataWrite, SdpmmPipe = Value
  }

  val kvDataReady = RegInit(true.B)
  mm2s_k.io.streamOut.ready := kvDataReady
  mm2s_v.io.streamOut.ready := kvDataReady

  val kvDataValid = RegInit(false.B)
  s2mm_k.io.streamIn.bits.data := kLine
  s2mm_v.io.streamIn.bits.data := vLine
  s2mm_k.io.streamIn.valid := kvDataValid
  s2mm_v.io.streamIn.valid := kvDataValid

  val kvLineLastReg = RegInit(false.B)
  s2mm_k.io.streamIn.bits.last := kvLineLastReg
  s2mm_v.io.streamIn.bits.last := kvLineLastReg

  val kvCnt = Counter(SdpmmConfigs.L + 1)
  val outCnt = Counter(SdpmmConfigs.L + 1)

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

  // 32 * 16 + 32 * maskType(16) = 1024
  val inPipeData = Wire(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask))

  val lastReg = Reg(Bool())
  lastReg := false.B

  // data prepare
  val tState = RegInit(state.kvIdle)
  inPipeData := mm2s_pipe.io.streamOut.bits.data.asTypeOf(chiselTypeOf(inPipeData))
  sdpmmModule.InputPipe.bits := inPipeData
  sdpmmModule.InputPipe.valid := mm2s_pipe.io.streamOut.valid && (tState === state.SdpmmPipe)
  mm2s_pipe.io.streamOut.ready := sdpmmModule.InputPipe.ready && (tState === state.SdpmmPipe)

  sdpmmModule.OutputPipe.ready := s2mm_pipe.io.streamIn.ready
  s2mm_pipe.io.streamIn.valid := sdpmmModule.OutputPipe.valid
  s2mm_pipe.io.streamIn.bits.data := sdpmmModule.OutputPipe.bits.asUInt

  s2mm_pipe.io.streamIn.bits.last := lastReg

  val allStreamFinished = RegInit(false.B)

  io.done := readReqIssuedReg && writeReqIssuedReg && !mm2s_pipe.io.busy && !s2mm_pipe.io.busy &&
    kReadReqIssuedReg && kWriteReqIssuedReg && !mm2s_k.io.busy && !s2mm_k.io.busy && vReadReqIssuedReg &&
    vWriteReqIssuedReg && !mm2s_v.io.busy && !s2mm_v.io.busy && allStreamFinished

  debugLog(
    p"=========\n tState  = ${tState}, kvDataValid = ${kvDataValid}, kvDataReady = ${kvDataReady}\n==========\n"
  )

  // the last flag should go with the last valid
  kvLineLastReg := Mux(outCnt.value >= (SdpmmConfigs.L - 1).U, true.B, false.B)
  val inputNumTimes = RegInit(0.U(32.W))
  inputNumTimes := io.dataIF.inputNumTimes
  val inputCnt = common.counter(inputNumTimes + 1.U, sdpmmModule.OutputPipe.fire)
  lastReg := Mux(inputCnt >= inputNumTimes - 1.U, true.B, false.B)

  switch(tState) {
    is(state.kvIdle) {
      debugLog(p"state.kvIdle\n")
      when(mm2s_k.io.streamOut.valid && mm2s_v.io.streamOut.valid && kvCnt.value < SdpmmConfigs.L.U) {
        debugLog(p"go to kvDataRead\n")
        tState := state.kvDataRead
        kvDataReady := false.B
      }

      when(outCnt.value === SdpmmConfigs.L.U && inputCnt < inputNumTimes) {
        debugLog(p"go to SdpmmPipe\n")
        // kvLineLastReg := true.B
        tState := state.SdpmmPipe
      }
    }

    is(state.kvDataRead) {
      debugLog(p"state.kvDataRead\n")
      debugLog(p"The KLine is: ${kLine},\n The VLine is: ${vLine}\n")
      // debugLog(p"The KIntReg is: ${kIntReg},\n The VIntReg is: ${vIntReg}\n")
      kIntReg := (kLine + (kIntReg.asUInt << common.DATA_WIDTH)).asTypeOf(kIntReg)
      vIntReg := (vLine + (vIntReg.asUInt << common.DATA_WIDTH)).asTypeOf(vIntReg)
      kvCnt.inc()
      tState := state.kvDataWrite
    }

    is(state.kvDataWrite) {
      debugLog(p"state.kvDataWrite\n")
      kvDataValid := true.B
      when(kvDataValid && s2mm_k.io.streamIn.ready && s2mm_v.io.streamIn.ready) {
        debugLog(p"Consume a kvLine Data\n")
        kvDataValid := false.B
        kvDataReady := true.B
        outCnt.inc()
        tState := state.kvIdle
      }
    }

    // TODO: need do it as a flow
    is(state.SdpmmPipe) {
      // need turn to idle
      debugLog(p"state.SdpmmPipe: \n")
      // debugLog(p"The KIntReg is: ${kIntReg},\n The VIntReg is: ${vIntReg}\n")
      // debug
      debugLog(
        p"mm2s_pipe.io.busy=${mm2s_pipe.io.busy} s2mm_pipe.io.busy=${s2mm_pipe.io.busy} " +
          p"kReadReqIssuedReg=${kReadReqIssuedReg} kWriteReqIssuedReg=${kWriteReqIssuedReg} " +
          p"mm2s_k.io.busy=${mm2s_k.io.busy} s2mm_k.io.busy=${s2mm_k.io.busy} " +
          p"vReadReqIssuedReg=${vReadReqIssuedReg} vWriteReqIssuedReg=${vWriteReqIssuedReg} " +
          p"mm2s_v.io.busy=${mm2s_v.io.busy} s2mm_v.io.busy=${s2mm_v.io.busy}\n"
      )

      debugLog(p"The SdpmmOutput.Valid is ${sdpmmModule.OutputPipe.valid}\n")

      when(inputCnt >= inputNumTimes) {
        debugLog(p"Finish the SdpmmPipe\n")
        tState := state.kvIdle
        allStreamFinished := true.B
      }

    }
  }

}
