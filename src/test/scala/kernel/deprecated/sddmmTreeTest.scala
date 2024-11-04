package kernel.deprecated

import kernel._
import chisel3._
import chisel3.experimental.BundleLiterals._

import scala.math._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class sddmmTreeTest extends AnyFreeSpec with Matchers {
  val dim = 86

  "VecDotVec should calculate proper greatest common denominator" in {
    simulate(new VecDotVecTree(dim)) { dut =>
      val rnd = new scala.util.Random(0)
      val testValues = (1 to dim).map(_ => (1, 1))
      val inputSeqQ = testValues.map { case (x, _) => (x * rnd.nextInt(10)) }
      println(inputSeqQ)
      //      val inputSeqQ = testValues.map { case (x, y) => (x) }
      val inputSeqK = testValues.map { case (_, y) => (y * rnd.nextInt(10)) }
      println(inputSeqK)
      //      val inputSeqK = testValues.map { case (x, y) => (y) }
      val resultSeq = inputSeqQ.zip(inputSeqK).map { case (a, b) => a * b }.sum
      println(resultSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (i <- 0 until dim) {
        dut.io.rowQ(i).poke(inputSeqQ(i).U)
        dut.io.rowK(i).poke(inputSeqK(i).U)
      }

      val clkTime: Int = math.ceil(math.log10(dim) / log10(2)).toInt

      for (i <- 0 until clkTime) {
        dut.clock.step()
      }

      dut.io.res.expect(resultSeq.U)
    }
  }
}
