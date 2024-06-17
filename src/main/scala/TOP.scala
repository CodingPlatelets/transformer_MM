import chisel3._
import chisel3.stage._
import circt.stage.ChiselStage
import pe._

object TOP extends App {
  ChiselStage.emitSystemVerilogFile(
    // new SdpmmOrigin(16, 8, 64, 8, 10, "",64),
    new Sdpmm(16, 16, 128, 16, 10),
    // new ForwardingMemory(32, 1024),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info"),
    args = Array("--target-dir", "chisel_output")
  )

  // (new circt.stage.ChiselStage).execute(
  //   Array("--target", "verilog") ++ args,
  //   Seq(
  //     circt.stage.CIRCTTargetAnnotation(circt.stage.CIRCTTarget.Verilog),
  //     ChiselGeneratorAnnotation(() =>
  //       //      new VecDotVec(8)),
  //       //        new SeqDotVecs(3, 8)
  //       new Sdpmm(16, 8, 64, 8, 10)
  //     ),
  //     circt.stage.FirtoolOption("-disable-all-randomization"),
  //     circt.stage.FirtoolOption("-strip-debug-info"),
  //   )
  // )
}
