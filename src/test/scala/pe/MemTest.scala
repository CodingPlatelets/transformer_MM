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

  behavior.of("Mem")
  it should ("Forward Mem test") in {
    test(new ForwardingMemory(bit, dimV)).withAnnotations(annos) { dut =>
      val testQ = Seq.tabulate(dimV)(i => 2)
      val testQ2 = Seq.tabulate(dimV)(i => 3)
      dut.io.wrAddr.poke(1.U)
      dut.io.wrEna.poke(true.B)
      for (i <- 0 until dimV) {
        dut.io.wrData(i).poke(testQ(i).U)
      }

      dut.clock.step()

      dut.io.wrAddr.poke(2.U)
      dut.io.wrEna.poke(true.B)
      for (i <- 0 until dimV) {
        dut.io.wrData(i).poke(testQ2(i).U)
      }

      dut.clock.step()
      parallel(dut.io.rdAddr.poke(1.U), dut.io.wrEna.poke(false.B))

      dut.clock.step()
      for (i <- 0 until dimV) {
        dut.io.rdData(i).expect(testQ(i).U)
      }

      dut.io.rdAddr.poke(2.U)

      dut.clock.step()
      for (i <- 0 until dimV) {
        dut.io.rdData(i).expect(testQ2(i).U)
      }
      println("yes")

    }
  }

}
