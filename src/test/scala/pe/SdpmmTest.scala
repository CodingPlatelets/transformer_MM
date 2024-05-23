package pe

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class SdpmmTest extends AnyFreeSpec with Matchers {
  val bit = 32
  val dimV = 36
  val L = 40
  val numOfMask = 4
  val queueSize = 5
  "sdpmm should calculate in lines" in {
    simulate(new sdpmm(bit, dimV, L, numOfMask, queueSize)) { dut =>
      // sddmm
      val testQ = for (i <- 0 until dimV) yield (i)
      val mask1 = Seq(0, 2, 4, 17)
      val mask2 = Seq(1, 3, 5, 16)
      val mask3 = Seq(3, 4, 6, 19)
      val mask = Seq(mask1, mask2, mask3)

      val testK =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j)

      val testV =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j + 1)

      val temp1 = Array.fill(L)(0)
      val temp2 = Array.fill(L)(0)
      val temp3 = Array.fill(L)(0)
      for (i <- 0 until numOfMask) {
        temp1(mask1(i)) = testQ.zip(testK(mask1(i))).map { case (a, b) => a * b }.sum
        temp2(mask2(i)) = testQ.zip(testK(mask2(i))).map { case (a, b) => a * b }.sum
        temp3(mask3(i)) = testQ.zip(testK(mask3(i))).map { case (a, b) => a * b }.sum
      }

      val temp = Seq(temp1, temp2, temp3)

      // spmm
      var res01 = Array.fill(dimV)(0)
      var res02 = Array.fill(dimV)(0)
      var res03 = Array.fill(dimV)(0)

      for (i <- 0 until numOfMask) {
        for (j <- 0 until dimV) {
          res01(j) = res01(j) + temp1(mask1(i)) * testV(mask1(i))(j)
          res02(j) = res02(j) + temp2(mask2(i)) * testV(mask2(i))(j)
          res03(j) = res03(j) + temp3(mask3(i)) * testV(mask3(i))(j)
        }
      }

      val res = Seq(res01, res02, res03)

      println("res01 is " + res01.toIndexedSeq)
      println("res02 is " + res02.toIndexedSeq)
      println("res03 is " + res03.toIndexedSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.kMatrix(j)(i).poke(testK(j)(i).U)
        }
      }

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.vMatrix(j)(i).poke(testV(j)(i).U)
        }
      }
      dut.clock.step()

      var cnt = 0
      while (cnt < mask.length) {
        if (dut.io.inMask.ready.peek().litToBoolean && dut.io.nums.ready.peek().litToBoolean) {
          for (i <- 0 until numOfMask) {
            dut.io.inMask.bits(i).poke(mask(cnt)(i).U)
          }

          for (i <- 0 until dimV) {
            dut.io.nums.bits(i).poke(testQ(i).U)
          }

          dut.io.inMask.valid.poke(true.B)
          dut.io.nums.valid.poke(true.B)
          cnt = cnt + 1
        }
        dut.clock.step()
      }

      dut.io.inMask.valid.poke(false.B)
      dut.io.nums.valid.poke(false.B)

      cnt = 0
      while (cnt < mask.length) {
        dut.io.res.ready.poke(false.B)
        dut.io.outMask.ready.poke(false.B)
        if (dut.io.res.valid.peek().litToBoolean) {
          for (i <- 0 until dimV) {
            dut.io.res.bits(i).expect(res(cnt)(i).U)
          }
          dut.io.outMask.valid.expect(true.B)
          for (i <- 0 until numOfMask) {
            dut.io.outMask.bits(i).expect(mask(cnt)(i).U)
          }
          dut.io.outMask.ready.poke(true.B)
          dut.io.res.ready.poke(true.B)
          cnt = cnt + 1
        }
        dut.clock.step()
      }

      dut.io.res.ready.poke(false.B)
      dut.io.outMask.ready.poke(false.B)
    }
  }
}
