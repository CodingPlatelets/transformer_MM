package vitiskernel

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import pe.TOPSdpmm
import pe.utils._
import vitiskerneldata.VitisRTLKernelDataIF

class VitisRTLKernel extends RawModule {

  val ap_clk = IO(Input(Clock()))
  val reset_asyncReset = Wire(new AsyncReset)
  // Step2: Instantiate your kernel here
  val kernel = withClockAndReset(ap_clk, reset_asyncReset)(Module(new TOPSdpmm))

  // !! DO NOT modify code below !!
  val ap_start = IO(Input(Bool()))
  val ap_idle = IO(Output(Bool()))
  val ap_done = IO(Output(Bool()))
  val ap_ready = IO(Output(Bool()))

  val dataIF = IO(new VitisRTLKernelDataIF)
  dataIF <> kernel.io.dataIF

  ap_idle := false.B
  ap_done := false.B
  ap_ready := false.B

  val reset_w = Wire(Bool())
  reset_asyncReset := reset_w.asAsyncReset
  reset_w := false.B

  val sIdle :: sReset1 :: sReset2 :: sBusy :: sDone :: Nil = Enum(5)

  val apStartN_asyncReset = Wire(new AsyncReset)
  apStartN_asyncReset := (!ap_start).asAsyncReset

  val state_r = withClockAndReset(ap_clk, apStartN_asyncReset)(RegInit(sIdle))

  switch(state_r) {
    is(sIdle) {
      ap_idle := true.B
      reset_w := true.B
      when(ap_start) {
        state_r := sReset1
      }
    }
    is(sReset1) {
      reset_w := true.B
      state_r := sReset2
    }
    is(sReset2) {
      reset_w := true.B
      state_r := sBusy
    }
    is(sBusy) {
      when(kernel.io.done) {
        state_r := sDone
      }
    }
    is(sDone) {
      ap_done := true.B
      ap_ready := true.B
      state_r := sIdle
    }
  }
}

object VitisRTLKernelVerilog extends App {
//  args.foreach(x =println(x))
  ChiselStage.emitSystemVerilogFile(
    // new VitisRTLKernel,
    new Float2Fxp,
    // firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info"),
    firtoolOpts = Array("-disable-all-randomization"),
    args = Array("--target-dir", "./build/chisel")
  )
}
