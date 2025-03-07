package kernel.deprecated

import chisel3._
import chisel3.experimental.BundleLiterals._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class PETest extends AnyFreeSpec with Matchers {
  val dim = 8
  val bit = 8
  val nums = 3

  "PE SPMM should calculate proper MAC" taggedAs deprecated in {
    simulate(new PE(bit, (0, 0))) { dut =>
      val testLeft = (2 to 2 + nums)
      val testTOP = (4 to 4 + nums)
      val testRes = testLeft.zip(testTOP).map { case (a, b) => a * b }.sum

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      val temp = Seq(0, 8, 23, 47)
      val reg = Seq(8, 23, 47, 82)
      var t = 0

      for (i <- 0 until nums) {
        dut.io.inLeft.poke(testLeft(i).U)
        dut.io.inTop.poke(testTOP(i).U)
        dut.io.inReg.poke(temp(i).U)
        dut.io.controlSign.poke(ControlSignalSel.SPMM)

        dut.clock.step()
        dut.io.outReg.expect(reg(i).U)
      }
    }
  }

  "PE SDDMM should calculate proper MAC" taggedAs deprecated in {
    simulate(new PE(bit, (0, 0))) { dut =>
      val testLeft = (2 to 2 + nums)
      val testTOP = (4 to 4 + nums)
      val testRes = testLeft.zip(testTOP).map { case (a, b) => a * b }.sum
      val testRight = (2 to 2 + nums)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      val temp = Seq(0, 8, 23, 47)
      val reg = Seq(8, 23, 47, 82)

      var t = 0

      for (i <- 0 until nums) {
        if (i == 0) {
          dut.io.outRight.expect(0.U)
        } else {
          dut.io.outRight.expect(testRight(i - 1).U)
        }
        dut.io.inLeft.poke(testLeft(i).U)
        dut.io.inTop.poke(testTOP(i).U)
        dut.io.inReg.poke(temp(i).U)
        dut.io.controlSign.poke(ControlSignalSel.SDDMM)

        dut.clock.step()
        dut.io.outReg.expect(reg(i).U)
        dut.io.outRight.expect(testRight(i).U)
      }

    }
  }
}
