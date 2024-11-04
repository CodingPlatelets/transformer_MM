package kernel.deprecated

import chisel3._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class findOneHotTest extends AnyFreeSpec with Matchers {
  "findOneHot should calculate properly" in {
    simulate(new findOneHot) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      dut.in.poke("b100010001".asUInt)
      dut.clock.step()
      dut.out.expect("b00000001".asUInt)
    }
  }
}
