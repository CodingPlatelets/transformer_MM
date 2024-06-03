package pe

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
class MemTest extends AnyFlatSpec with ChiselScalatestTester {
  val bit = 64
  // val dimV = 32
  val dimV = 64
  // val L = 64
  val L = 96
  // val numOfMask = 8
  val numOfMask = 64
  val queueSize = 10
  val inPutTimes = 3
  val fileName = "src/main/resources/memtest.txt"
  val annos = Seq(VerilatorBackendAnnotation)

  val delay = 3
  behavior.of("delay Mem")
  it should ("Sequence delay n cycles") in {
    test(new ForwardingDelayMemory(UInt(16.W), 16, delay)).withAnnotations(annos) { dut =>
      dut.io.wrData.initSource()
      dut.io.rdData.initSink()

      fork {
        dut.io.wrData.enqueueSeq(Seq.tabulate(delay)(i => i.U))
      }.fork {
        dut.io.rdData.expectDequeueSeq(Seq.tabulate(delay)(i => i.U))
      }.join()
    }
  }

  it should ("delay n cycles") in {
    test(new ForwardingDelayMemory(UInt(16.W), 16, delay)).withAnnotations(annos) { dut =>
      dut.io.wrData.initSource()
      dut.io.rdData.initSink()

      dut.io.wrData.enqueueNow(1.U)
      dut.io.rdData.expectInvalid()
      dut.clock.step(5)
      dut.io.wrData.enqueueNow(2.U)

      dut.io.rdData.expectInvalid()
      dut.clock.step(5)

      dut.io.wrData.enqueueNow(3.U)
      dut.io.rdData.expectInvalid()
      dut.clock.step(4)

      dut.io.rdData.expectDequeueNow(1.U)
      dut.clock.step(3)
      dut.io.rdData.expectDequeueNow(2.U)

      dut.clock.step(3)
      dut.io.rdData.expectDequeueNow(3.U)
    }
  }
}
