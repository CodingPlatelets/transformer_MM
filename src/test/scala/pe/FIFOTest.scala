package pe

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import pe.utils.QueueModule

class FIFOTest extends AnyFlatSpec with ChiselScalatestTester {

  val bit = 64
  val dimV = 32
  val depth = 128
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fifo with memory")
  it should "fifo with it" in {
    test(new QueueModule(UInt(bit.W), entries = depth, useMem = true, pipe = false, flow = false))
      .withAnnotations(annos) { dut =>
        dut.in.initSource()
        dut.out.initSink()

        fork {
          dut.in.enqueueSeq(Seq(1.U, 2.U, 3.U))
        }.fork {
          dut.out.expectDequeueSeq(Seq(1.U, 2.U, 3.U))
        }.join()
      }
  }
}
