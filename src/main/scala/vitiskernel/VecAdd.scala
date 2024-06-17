package vitiskernel

import chisel3._
import chisel3.util._
import vitiskernel.mmstream._

class VecAdd extends Module {

  // kernel 的 IO是固定的，不要增加其他 port
  val io = IO(new Bundle {
    val dataIF = (new VitisRTLKernelDataIF)
    val done = Output(Bool())
  })

  // 在 reset 直接开始执行，执行结束后将 done 置位即可
  val readReqIssued_reg = RegInit(false.B)
  val writeReqIssued_reg = RegInit(false.B)

  val mm2s_module = Module(new MM2S(64, 512))
  val s2mm_module = Module(new S2MM(64, 512))

  mm2s_module.io.axiRead <> io.dataIF.m00Read
  s2mm_module.io.axiWrite <> io.dataIF.m00Write
  mm2s_module.io.req.bits.addr := io.dataIF.readAddress
  mm2s_module.io.req.bits.len := io.dataIF.readLength
  s2mm_module.io.req.bits.addr := io.dataIF.writeAddress

  mm2s_module.io.req.valid := !readReqIssued_reg
  s2mm_module.io.req.valid := !writeReqIssued_reg
  when(mm2s_module.io.req.ready) {
    readReqIssued_reg := true.B
  }
  when(s2mm_module.io.req.ready) {
    writeReqIssued_reg := true.B
  }

  io.done := readReqIssued_reg && writeReqIssued_reg && !mm2s_module.io.busy && !s2mm_module.io.busy

  s2mm_module.io.streamIn.valid := mm2s_module.io.streamOut.valid
  mm2s_module.io.streamOut.ready := s2mm_module.io.streamIn.ready
  val inputData_wire = Wire(Vec(16, UInt(32.W)))
  val outputData_wire = Wire(Vec(16, UInt(32.W)))
  inputData_wire := mm2s_module.io.streamOut.bits.data.asTypeOf(inputData_wire)

  outputData_wire
    .zip(inputData_wire)
    .foreach(p => {
      p._1 := p._2 + 47.U
    })
  s2mm_module.io.streamIn.bits.data := outputData_wire.asUInt
  s2mm_module.io.streamIn.bits.last := mm2s_module.io.streamOut.bits.last

}
