package pe

import chisel3._
import chisel3.util._

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers



class validReadyTest extends AnyFreeSpec with Matchers {
  "validReady should calculate properly" in {
    simulate(new ValidReady) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      println("test validReady")

      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.poke("b10001000".asUInt)
      dut.clock.step(3)

      dut.io.out.bits.expect("b10001010".asUInt)

    }
  }
}
