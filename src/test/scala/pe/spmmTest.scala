package pe

import chisel3._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
class spmmTest extends AnyFreeSpec with Matchers {

  val bit = 16
  val dimV = 8
  val L = 28
  "spmm should calculate in lines" in {
    simulate(new spmm(bit, dimV, L)) { dut =>
      var mask = for (i <- 0 until L) yield for (j <- 0 until L) yield {
        if (j % 9 == 0) true else false
      }

      var nums01 = for (i <- 0 until L) yield { if (i % 9 == 0) i + (i + 1) else 0 }
      var nums02 = for (i <- 0 until L) yield { if (i % 9 == 0) 5 + (i * 2) else 0 }
      var nums03 = for (i <- 0 until L) yield { if (i % 9 == 0) 4 + (i * 3) else 0 }

      var vMatrix = for (i <- 1 to L) yield {
        if ((i - 1) % 9 == 0) for (j <- 1 to dimV) yield {
          2 * i + j
        }
        else for (j <- 1 to dimV) yield 0
      }

      var res01 = Array.fill(dimV)(0)
      var res02 = Array.fill(dimV)(0)
      var res03 = Array.fill(dimV)(0)

      for (i <- 0 until L) {
        if (i % 9 == 0) {
          for (j <- 0 until dimV) {
            res01(j) = res01(j) + nums01(i) * vMatrix(i)(j)
            res02(j) = res02(j) + nums02(i) * vMatrix(i)(j)
            res03(j) = res03(j) + nums03(i) * vMatrix(i)(j)
          }
        }
      }

      val testRes01 = res01.map(x => x.U(bit.W))
      val testRes02 = res02.map(x => x.U(bit.W))
      val testRes03 = res03.map(x => x.U(bit.W))

      val numOfMask = L / 9 + (if (L % 9 == 0) 0 else 1)

      // println("mask is " + mask)
      println("nums01 is " + nums01)
      println("numOfMask is " + numOfMask)
      // println("vMatrix is " + vMatrix)
      println("res01 is " + res01.toIndexedSeq)
      println("res02 is " + res02.toIndexedSeq)
      println("res03 is " + res03.toIndexedSeq)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      // first cycle
      dut.clock.step()

      for (i <- 0 until L) {
        dut.io.nums.bits(i).poke(nums01(i).U)
        for (j <- 0 until L) {
          dut.io.mask(i)(j).poke(mask(i)(j).B)
        }
        for (m <- 0 until dimV) {
          dut.io.vMatrix(i)(m).poke(vMatrix(i)(m).U)
        }
      }
      dut.io.numOfMask.poke(numOfMask.U)
      dut.io.nums.valid.poke(true.B)
      dut.io.res.ready.poke(true.B)
      dut.clock.step(numOfMask + 1)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until dimV) {
        dut.io.res.bits(i).expect(testRes01(i))
      }

      // another cycle
      dut.clock.step()
      for (i <- 0 until L) {
        dut.io.nums.bits(i).poke(nums02(i).U)
      }
      dut.io.numOfMask.poke(numOfMask.U)
      dut.io.nums.valid.poke(true.B)
      dut.io.res.ready.poke(true.B)
      dut.clock.step(numOfMask)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until dimV) {
        dut.io.res.bits(i).expect(testRes02(i))
      }

      // for last cycle
      dut.clock.step()
      for (i <- 0 until L) {
        dut.io.nums.bits(i).poke(nums03(i).U)
      }
      dut.io.numOfMask.poke(numOfMask.U)
      dut.io.nums.valid.poke(true.B)
      dut.io.res.ready.poke(true.B)
      dut.clock.step(numOfMask)

      dut.io.res.valid.expect(true.B)
      for (i <- 0 until dimV) {
        dut.io.res.bits(i).expect(testRes03(i))
      }
    }
  }

}
