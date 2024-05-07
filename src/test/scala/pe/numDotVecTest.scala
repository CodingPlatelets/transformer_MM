package pe

import chisel3._
import chisel3.experimental.BundleLiterals._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class NumDotVecTest extends AnyFreeSpec with Matchers {
  val dim = 8
  val bit = 8
  val numOfMask = 3

  "numDotVec should calculate proper" in {
    simulate(new NumDotVec(bit, 0, dim)) { dut =>
      val testLeft = for (i <- 1 to numOfMask) yield { i }
      val testTOP = for (k <- 1 to numOfMask) yield {
        for (i <- 1 to dim) yield {
          k * i * 2
        }
      }
      // calculate the result of the test: each testLeft * testTOP(i) and finally sum them as a vec with dim elements
      var temp = Array.fill(dim)(0)
      for (i <- 0 until numOfMask) {
        for (j <- 0 until dim) {
          temp(j) = temp(j) + testLeft(i) * testTOP(i)(j)
        }
      }
      val testRes = temp.map(x => x.U((2 * bit).W))

      println("testLeft is " + testLeft)
      println("testTOP is " + testTOP)
      println("testRes is " + temp.toIndexedSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.io.numOfMask.poke(numOfMask.U)
      var cnt = 0
      while (cnt < numOfMask) {
        dut.io.num.valid.poke(true.B)
        dut.io.vec.valid.poke(true.B)

        if (dut.io.num.ready.peek().litToBoolean && dut.io.vec.ready.peek().litToBoolean) {
          dut.io.num.bits.poke(testLeft(cnt).U)
          for (j <- 0 until dim) {
            dut.io.vec.bits(j).poke(testTOP(cnt)(j).U)
          }
          cnt += 1
        }

        dut.clock.step()

      }

      dut.io.res.valid.expect(true.B)
      dut.io.num.ready.expect(false.B)
      dut.io.vec.ready.expect(false.B)
      for (i <- 0 until dim) {
        dut.io.res.bits(i).expect(testRes(i))
      }

      // Test for next alu cycle
      // dut.clock.step()
      cnt = 0
      while (cnt < numOfMask) {
        dut.io.num.valid.poke(true.B)
        dut.io.vec.valid.poke(true.B)

        if (dut.io.num.ready.peek().litToBoolean && dut.io.vec.ready.peek().litToBoolean) {
          dut.io.num.bits.poke(testLeft(cnt).U)
          for (j <- 0 until dim) {
            dut.io.vec.bits(j).poke(testTOP(cnt)(j).U)
          }
          cnt += 1
        }

        dut.clock.step()
      }

      dut.io.res.valid.expect(true.B)
      dut.io.num.ready.expect(false.B)
      dut.io.vec.ready.expect(false.B)
      for (i <- 0 until dim) {
        dut.io.res.bits(i).expect(testRes(i))
      }

      // Test for last alu cycle
      // dut.clock.step()
      cnt = 0
      while (cnt < numOfMask) {
        dut.io.num.valid.poke(true.B)
        dut.io.vec.valid.poke(true.B)

        if (dut.io.num.ready.peek().litToBoolean && dut.io.vec.ready.peek().litToBoolean) {
          dut.io.num.bits.poke(testLeft(cnt).U)
          for (j <- 0 until dim) {
            dut.io.vec.bits(j).poke(testTOP(cnt)(j).U)
          }
          cnt += 1
        }

        dut.clock.step()
      }

      dut.io.res.valid.expect(true.B)
      dut.io.num.ready.expect(false.B)
      dut.io.vec.ready.expect(false.B)
      for (i <- 0 until dim) {
        dut.io.res.bits(i).expect(testRes(i))
      }
    }
  }
}
