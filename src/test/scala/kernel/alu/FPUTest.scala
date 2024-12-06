package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class FCMAAddTest extends AnyFlatSpec with ChiselScalatestTester {
  "FCMAAdd" should "correctly perform multiply-add operation" in {
    test(new fudian.FCMA(8, 24)) { dut =>
      dut.io.a.poke("h3f800000".U) // Example input a (1.0 in IEEE 754 single precision)
      dut.io.b.poke("h40000000".U) // Example input b (2.0 in IEEE 754 single precision)
      dut.io.c.poke("h40400000".U) // Example input c (3.0 in IEEE 754 single precision)
      dut.io.rm.poke(0.U) // Example rounding mode
      dut.clock.step(1)
      dut.io.result.expect("h40a00000".U) // Expected output (5.0 in IEEE 754 single precision)
    }
  }
}
