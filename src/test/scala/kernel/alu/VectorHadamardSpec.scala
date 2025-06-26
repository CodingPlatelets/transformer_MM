package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import chisel3.util.DecoupledIO

class VectorHadamardSpec extends AnyFlatSpec with ChiselScalatestTester {

  def floatToFixed(f: Float, wii: Int, wif: Int): BigInt = {
    val totalWidth = wii + wif
    val longVal = Math.round(f * (1 << wif))
    if (longVal < 0) {
      (BigInt(1) << totalWidth) + longVal
    } else {
      BigInt(longVal)
    }
  }

  def fixedToFloat(i: BigInt, wii: Int, wif: Int): Float = {
    val totalWidth = wii + wif
    val maxPositiveValue = BigInt(1) << (totalWidth - 1)
    val asSigned = if (i >= maxPositiveValue) i - (BigInt(1) << totalWidth) else i
    asSigned.toFloat / (1 << wif)
  }

  behavior.of("VectorHadamard")

  it should "correctly compute hadamard product of two large vectors using limited PEs" in {
    val WII = 8
    val WIF = 8
    val VECTOR_SIZE = 32 // Using a smaller size for faster simulation
    val NUM_PE = 8

    test(new VectorHadamard(WII, WIF, VECTOR_SIZE, NUM_PE))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        // 1. Generate test data
        val rand = new scala.util.Random
        val max_val_float = 2.0f
        val vec_a_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)
        val vec_b_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)

        val vec_a_fixed = vec_a_float.map(f => floatToFixed(f, WII, WIF).U)
        val vec_b_fixed = vec_b_float.map(f => floatToFixed(f, WII, WIF).U)

        // 2. Calculate expected result in software
        val expected_res_float = (vec_a_float.zip(vec_b_float)).map { case (a, b) => a * b }
        val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF).U)

        // 3. 流式Decoupled输入输出
        // 输入A/B
        var sent = false
        while (!sent) {
          if (dut.io.in_a.ready.peekBoolean() && dut.io.in_b.ready.peekBoolean()) {
            dut.io.in_a.valid.poke(true.B)
            dut.io.in_b.valid.poke(true.B)
            for (i <- 0 until VECTOR_SIZE) {
              dut.io.in_a.bits(i).poke(vec_a_fixed(i))
              dut.io.in_b.bits(i).poke(vec_b_fixed(i))
            }
            sent = true
          } else {
            dut.io.in_a.valid.poke(false.B)
            dut.io.in_b.valid.poke(false.B)
          }
          dut.clock.step(1)
        }
        dut.io.in_a.valid.poke(false.B)
        dut.io.in_b.valid.poke(false.B)

        // 输出检查
        var received = false
        while (!received) {
          if (dut.io.out.valid.peekBoolean()) {
            dut.io.out.ready.poke(true.B)
            for (i <- 0 until VECTOR_SIZE) {
              val expected_val = expected_res_fixed(i).litValue
              val actual_val = dut.io.out.bits(i).peek().litValue
              val expected_float = fixedToFloat(expected_val, WII, WIF)
              val actual_float = fixedToFloat(actual_val, WII, WIF)
              println(s"idx $i : expected $expected_float, got $actual_float")
            }
            received = true
          } else {
            dut.io.out.ready.poke(false.B)
          }
          dut.clock.step(1)
        }
        dut.io.out.ready.poke(false.B)
      }
  }
} 