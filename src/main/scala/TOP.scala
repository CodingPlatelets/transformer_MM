import chisel3._
import chisel3.stage._
import _root_.circt.stage.ChiselStage
import pe._

object TOP extends App {
  ChiselStage.emitSystemVerilogFile(
    //    new SeqDotVecs(3,8),
    //  new VecDotVec(8),
    // new NumDotVec(4, 1, 4),
    new spmm(8, 4, 7),
    // new counterMux1H,
    // new testMux,
    // new PE(4, (1, 1), 0),
    firtoolOpts = Array("-disable-all-randomization")
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
