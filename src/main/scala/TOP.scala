import chisel3._
import chisel3.stage._
import _root_.circt.stage.ChiselStage
import pe._

object TOP extends App {
  ChiselStage.emitSystemVerilogFile(
    //    new SeqDotVecs(3,8),
    //  new VecDotVec(8),
    // new NumDotVec(4, 1, 4),
    // new SpMM(8, 8, 7, 3, 10),
    // new SdpmmOrigin(16, 8, 64, 8, 10, "",64),
    new Sdpmm(16, 16, 128, 16, 10),
    // new ForwardingMemory(32, 1024),
    // new Sddmm(8, 16, 32, 16, 10),
    // new RWmemFile(32, 1024, "/home/hwk/data/code/transformer_MM/src/main/resources/memtest.txt"),
    // new counterMux1H,
    // new testMux,
    // new PE(4, (1, 1), 0),
    // new VecDotVec(8, 32),
    // new sddmm(16, 8, 28, 4),
    // new ValidReady,
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
