package pe

import chisel3._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
class sddmmTest extends AnyFreeSpec with Matchers {
  val bit = 16
  val dimV = 8
  val L = 28
  val numOfMask = 4
  "sddmm should calculate in lines" in {
    simulate(new sddmm(bit, dimV, L, numOfMask)) { dut =>
      val testQ = for (i <- 0 until dimV) yield (i * 2)
      val mask = Seq(0, 2, 4, 7)
      val mask2 = Seq(1, 3, 5, 6)
      val mask3 = Seq(3, 4, 6, 7)
      val testK = for (i <- 0 until L) yield for (j <- 0 until dimV) yield (i + j)

      val res = Array.fill(L)(0)
      val res2 = Array.fill(L)(0)
      val res3 = Array.fill(L)(0)
      for (i <- 0 until numOfMask) {
        res(mask(i)) = testQ.zip(testK(mask(i))).map { case (a, b) => a * b }.sum
        res2(mask2(i)) = testQ.zip(testK(mask2(i))).map { case (a, b) => a * b }.sum
        res3(mask3(i)) = testQ.zip(testK(mask3(i))).map { case (a, b) => a * b }.sum
      }

      println(res.toIndexedSeq)
      println(res2.toIndexedSeq)
      println(res3.toIndexedSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      // first cycle
      dut.clock.step()

      for (i <- 0 until numOfMask) {
        dut.io.inMask.bits(i).poke(mask(i).U)
      }
      for (i <- 0 until dimV) {
        dut.io.qVec.bits(i).poke(testQ(i).U)
      }
      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.kMatrix(j)(i).poke(testK(j)(i).U)
        }
      }

      dut.io.inMask.ready.expect(true.B)
      dut.io.qVec.ready.expect(true.B)

      dut.io.inMask.valid.poke(true.B)
      dut.io.qVec.valid.poke(true.B)

      dut.clock.step()
      dut.io.inMask.valid.poke(false.B)
      dut.io.qVec.valid.poke(false.B)

      // dut.io.inMask.ready.expect(false.B)
      // dut.io.qVec.ready.expect(true.B)
      dut.clock.step(dimV)

      dut.io.res.valid.expect(true.B)
      dut.io.outMask.valid.expect(true.B)
      for (i <- 0 until numOfMask) {
        dut.io.res.bits(mask(i)).expect(res(mask(i)).U)
      }
      for (i <- 0 until numOfMask) {
        dut.io.outMask.bits(i).expect(mask(i).U)
      }

      dut.clock.step()
      dut.io.inMask.valid.poke(false.B)
      dut.io.qVec.valid.poke(false.B)
      dut.io.res.valid.expect(true.B)
      for (i <- 0 until numOfMask) {
        dut.io.res.bits(mask(i)).expect(res(mask(i)).U)
      }
      for (i <- 0 until numOfMask) {
        dut.io.outMask.bits(i).expect(mask(i).U)
      }

      dut.clock.step(5)
      dut.io.inMask.valid.poke(false.B)
      dut.io.qVec.valid.poke(false.B)
      dut.io.res.valid.expect(true.B)
      for (i <- 0 until numOfMask) {
        dut.io.res.bits(mask(i)).expect(res(mask(i)).U)
      }
      for (i <- 0 until numOfMask) {
        dut.io.outMask.bits(i).expect(mask(i).U)
      }

      // another cycle
      dut.clock.step()
      dut.io.inMask.valid.poke(true.B)
      dut.io.qVec.valid.poke(true.B)
      for (i <- 0 until numOfMask) {
        dut.io.inMask.bits(i).poke(mask2(i).U)
      }
      for (i <- 0 until dimV) {
        dut.io.qVec.bits(i).poke(testQ(i).U)
      }
      dut.clock.step()

      dut.io.inMask.valid.poke(false.B)
      dut.io.qVec.valid.poke(false.B)

      dut.clock.step(dimV - 1)

      // // last cycle ahead input
      // dut.io.inMask.valid.poke(true.B)
      // dut.io.qVec.valid.poke(true.B)
      // for (i <- 0 until numOfMask) {
      //   dut.io.inMask.bits(i).poke(mask3(i).U)
      // }
      // for (i <- 0 until dimV) {
      //   dut.io.qVec.bits(i).poke(testQ(i).U)
      // }

      dut.clock.step()
      dut.io.res.valid.expect(true.B)
      dut.io.outMask.valid.expect(true.B)
      for (i <- 0 until numOfMask) {
        dut.io.res.bits(mask2(i)).expect(res2(mask2(i)).U)
      }
      for (i <- 0 until numOfMask) {
        dut.io.outMask.bits(i).expect(mask2(i).U)
      }

    // // last cycle

    // dut.clock.step(dimV)

    // dut.io.res.valid.expect(true.B)
    // dut.io.outMask.valid.expect(true.B)
    // for (i <- 0 until numOfMask) {
    //   dut.io.res.bits(mask3(i)).expect(res3(mask3(i)).U)
    // }
    // for (i <- 0 until numOfMask) {
    //   dut.io.outMask.bits(i).expect(mask3(i).U)
    // }
    }
  }
}
