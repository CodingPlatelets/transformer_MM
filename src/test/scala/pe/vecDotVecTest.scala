package pe
import chisel3._
import chisel3.experimental.BundleLiterals._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class vecDotVecTest extends AnyFreeSpec with Matchers {
  val bit = 16
  val D = 63
  "vecDotVec should calculate properly" in {
    simulate(new VecDotVec(bit, D)) { dut =>
      val rnd = new scala.util.Random(0)
      val testValues = (1 to D).map(_ => (1, 1))
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
      dut.io.rowQ.valid.poke(true.B)
      dut.io.colK.valid.poke(true.B)
      for (i <- 0 until D) {
        dut.io.rowQ.bits(i).poke(inputSeqQ(i).U)
        dut.io.colK.bits(i).poke(inputSeqK(i).U)
      }
      dut.clock.step(D)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until D) {
        dut.io.res.bits.expect(resultSeq.U)
      }

      dut.io.colK.valid.poke(false.B)
      dut.io.rowQ.valid.poke(false.B)
      dut.clock.step(2)
      // another cycle
      dut.io.rowQ.valid.poke(true.B)
      dut.io.colK.valid.poke(true.B)
      for (i <- 0 until D) {
        dut.io.rowQ.bits(i).poke(inputSeqQ(i).U)
        dut.io.colK.bits(i).poke(inputSeqK(i).U)
      }
      dut.clock.step(D + 1)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until D) {
        dut.io.res.bits.expect(resultSeq.U)
      }
      // last cycle
      for (i <- 0 until D) {
        dut.io.rowQ.bits(i).poke(inputSeqQ(i).U)
        dut.io.colK.bits(i).poke(inputSeqK(i).U)
      }
      dut.clock.step(D + 1)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until D) {
        dut.io.res.bits.expect(resultSeq.U)
      }
      dut.clock.step()
      dut.io.res.valid.expect(false.B)
    }
  }
}
