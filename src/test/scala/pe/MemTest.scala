package pe

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class MemTest extends AnyFreeSpec with Matchers {
  val bit = 64
  // val dimV = 32
  val dimV = 64
  // val L = 64
  val L = 512
  // val numOfMask = 8
  val numOfMask = 64
  val queueSize = 10
  val inPutTimes = 3
  val fileName = "/home/hwk/data/code/transformer_MM/src/main/resources/memtest.txt"
  "mem should read in file" in {
    simulate(new RWmemFile(bit, dimV, fileName)) { dut =>
      val testQ = for (i <- 0 until dimV) yield (i)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.enable.poke(true.B)

      dut.io.write.poke(true.B)
      dut.io.addr.poke(1.U(10.W))
      // dut.io.dataIn.poke(1.U)
      for (i <- 0 until dimV) {
        dut.io.dataIn(i).poke(testQ(i).U)
      }
      dut.clock.step()
      dut.io.write.poke(false.B)

      dut.clock.step()
      // dut.io.dataOut.expect(1.U)
      for (i <- 0 until dimV) {
        dut.io.dataOut(i).expect(testQ(i).U)
      }

    }
  }
}
