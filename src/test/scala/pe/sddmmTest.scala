package pe

import chisel3._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
class sddmmTest extends AnyFreeSpec with Matchers {
  val bit = 32
  val dimV = 68
  val L = 28
  val numOfMask = 4
  val queueSize = 10
  "sddmm should calculate in lines" in {
    simulate(new sddmm(bit, dimV, L, numOfMask, queueSize)) { dut =>
      val testQ = for (i <- 0 until dimV) yield (i * 2)
      val mask1 = Seq(0, 2, 4, 7)
      val mask2 = Seq(1, 3, 5, 6)
      val mask3 = Seq(3, 4, 6, 7)
      val mask = Seq(mask1, mask2, mask3)

      val testK =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j)

      val res1 = Array.fill(L)(0)
      val res2 = Array.fill(L)(0)
      val res3 = Array.fill(L)(0)
      for (i <- 0 until numOfMask) {
        res1(mask1(i)) = testQ.zip(testK(mask1(i))).map { case (a, b) => a * b }.sum
        res2(mask2(i)) = testQ.zip(testK(mask2(i))).map { case (a, b) => a * b }.sum
        res3(mask3(i)) = testQ.zip(testK(mask3(i))).map { case (a, b) => a * b }.sum
      }

      val res = Seq(res1, res2, res3)

      println(res1.toIndexedSeq)
      println(res2.toIndexedSeq)
      println(res3.toIndexedSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.kMatrix(j)(i).poke(testK(j)(i).U)
        }
      }
      dut.clock.step()

      var cnt = 0
      while (cnt < mask.length) {
        if (dut.io.inMask.ready.peek().litToBoolean && dut.io.qVec.ready.peek().litToBoolean) {
          for (i <- 0 until numOfMask) {
            dut.io.inMask.bits(i).poke(mask(cnt)(i).U)
          }
          for (i <- 0 until dimV) {
            dut.io.qVec.bits(i).poke(testQ(i).U)
          }
          dut.io.inMask.valid.poke(true.B)
          dut.io.qVec.valid.poke(true.B)
          cnt += 1
        }
        dut.clock.step()
      }
      dut.io.inMask.valid.poke(false.B)
      dut.io.qVec.valid.poke(false.B)

      cnt = 0
      while(cnt < mask.length){
        dut.io.res.ready.poke(false.B)
        dut.io.outMask.ready.poke(false.B)

        if(dut.io.res.valid.peek().litToBoolean){
          dut.io.outMask.valid.expect(true.B)
          for (i <- 0 until numOfMask) {
            dut.io.res.bits(mask(cnt)(i)).expect(res(cnt)(mask(cnt)(i)).U)
            dut.io.outMask.bits(i).expect(mask(cnt)(i).U)
          }

          dut.io.outMask.ready.poke(true.B)
          dut.io.res.ready.poke(true.B)

          cnt += 1
        }

        dut.clock.step()
      }

      dut.io.res.ready.poke(false.B)
      dut.io.outMask.ready.poke(false.B)

    
    }
  }
}
