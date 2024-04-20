import chisel3._
import chisel3.stage._
import _root_.circt.stage.ChiselStage
import pe.PE

object TOP extends App {
  ChiselStage.emitSystemVerilogFile(
    //    new SeqDotVecs(3,8),
    //      new VecDotVec(8),
    new PE(1, 1, (1, 1), 0, 1),
    firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", "--verilog")
  )

  //  (new circt.stage.ChiselStage).execute(
  //    Array("--target", "verilog") ++ args,
  //    Seq(
  //      circt.stage.CIRCTTargetAnnotation(circt.stage.CIRCTTarget.Verilog),
  //      ChiselGeneratorAnnotation(() =>
  //        //      new VecDotVec(8)),
  ////        new SeqDotVecs(3, 8)
  //        new NumDotVec(8)
  //      ),
  ////      circt.stage.FirtoolOption("-disable-all-randomization"),
  ////      circt.stage.FirtoolOption("-strip-debug-info"),
  //    ),
  //  )
}
