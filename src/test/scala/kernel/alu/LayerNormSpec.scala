package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import chisel3.util.DecoupledIO
import _root_.circt.stage.ChiselStage
import chisel3.stage.ChiselGeneratorAnnotation

class ResADDSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** Converts a Float to a 2's complement fixed-point BigInt representation. */
  def floatToFixed(f: Float, wii: Int, wif: Int): BigInt = {
    val totalWidth = wii + wif
    val longVal = Math.round(f * (1 << wif))
    if (longVal < 0) {
      (BigInt(1) << totalWidth) + longVal
    } else {
      BigInt(longVal)
    }
  }

  /** Converts a 2's complement fixed-point BigInt back to a Float. */
  def fixedToFloat(i: BigInt, wii: Int, wif: Int): Float = {
    val totalWidth = wii + wif
    val maxPositiveValue = BigInt(1) << (totalWidth - 1)
    val asSigned = if (i >= maxPositiveValue) i - (BigInt(1) << totalWidth) else i
    asSigned.toFloat / (1 << wif)
  }

  behavior.of("ResADD")

  it should "correctly add two large vectors using limited PEs" in {
    val WII = 8
    val WIF = 8
    val VECTOR_SIZE = 32 // Using a smaller size for faster simulation
    val NUM_PE = 8

    test(new ResADD(WII, WIF, VECTOR_SIZE, NUM_PE))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        // 1. Generate test data
        val rand = new scala.util.Random
        val max_val_float = (1 << (WII - 2)).toFloat
        val vec_a_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)
        val vec_b_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)

        val vec_a_fixed = vec_a_float.map(f => floatToFixed(f, WII, WIF).U)
        val vec_b_fixed = vec_b_float.map(f => floatToFixed(f, WII, WIF).U)

        // 2. Calculate expected result in software
        val expected_res_float = (vec_a_float.zip(vec_b_float)).map { case (a, b) => a + b }
        val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF).U)

        // 3. 流式Decoupled输入输出

        // 输入A/B
        if (dut.io.x.ready.peekBoolean() && dut.io.residual.ready.peekBoolean()) {
          dut.io.x.valid.poke(true.B)
          dut.io.residual.valid.poke(true.B)
          for (i <- 0 until VECTOR_SIZE) {
            dut.io.x.bits(i).poke(vec_a_fixed(i))
            dut.io.residual.bits(i).poke(vec_b_fixed(i))
          }
        } else {
          dut.io.x.valid.poke(false.B)
          dut.io.residual.valid.poke(false.B)
        }
        dut.clock.step(1)
        while (!dut.io.addRes.valid.peekBoolean()) {
          dut.clock.step()
        }
        // 输出检查

        dut.io.addRes.ready.poke(true.B)
        for (i <- 0 until VECTOR_SIZE) {
          val expected_val = expected_res_fixed(i).litValue
          val actual_val = dut.io.addRes.bits(i).peek().litValue
          println(s"idx $i :expected $expected_val, got $actual_val")
          //   assert(expected_val == actual_val, s"Mismatch at index $i: expected $expected_val, got $actual_val")
        }

      }
  }

}

class LayerNormSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** Converts a Float to a 2's complement fixed-point BigInt representation. */
  def floatToFixed(f: Float, wii: Int, wif: Int): BigInt = {
    val totalWidth = wii + wif
    val longVal = Math.round(f * (1 << wif))
    if (longVal < 0) {
      (BigInt(1) << totalWidth) + longVal
    } else {
      BigInt(longVal)
    }
  }

  /** Converts a 2's complement fixed-point BigInt back to a Float. */
  def fixedToFloat(i: BigInt, wii: Int, wif: Int): Float = {
    val totalWidth = wii + wif
    val maxPositiveValue = BigInt(1) << (totalWidth - 1)
    val asSigned = if (i >= maxPositiveValue) i - (BigInt(1) << totalWidth) else i
    asSigned.toFloat / (1 << wif)
  }

  /** 计算LayerNorm的期望结果 */
  def calculateLayerNorm(x: Seq[Float], weight: Seq[Float], bias: Seq[Float]): Seq[Float] = {
    // 1. 计算均值
    val mean = x.sum / x.length

    // 2. 计算方差
    val variance = x.map(xi => math.pow(xi - mean, 2).toFloat).sum / x.length

    // 3. 计算标准差
    val stdDev = math.sqrt(variance).toFloat

    // 4. 归一化: (x - mean) / stdDev
    val normalized = x.map(xi => (xi - mean) / stdDev)

    // 5. 应用权重和偏置: weight * normalized + bias
    val result = (normalized.zip(weight).zip(bias)).map {
      case ((norm, w), b) => w * norm + b
    }

    result
  }

  behavior.of("LayerNorm")

  it should "correctly perform layer normalization on large vectors using limited PEs" in {
    val WII = 8
    val WIF = 8
    val VECTOR_SIZE = 32 // 使用较小的尺寸以加快仿真速度
    val NUM_PE = 8

    test(new LayerNorm(WII, WIF, VECTOR_SIZE, NUM_PE))
      .withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { dut =>
        // 1. 生成测试数据
        val rand = new scala.util.Random(42) // 固定种子以确保可重复性
        val max_val_float = 4.0f
        val vec_x_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)
        val vec_weight_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * 2.0f - 1.0f) // [-1, 1]
        val vec_bias_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * 0.5f - 0.25f) // [-0.25, 0.25]

        val vec_x_fixed = vec_x_float.map(f => floatToFixed(f, WII, WIF).U)
        val vec_weight_fixed = vec_weight_float.map(f => floatToFixed(f, WII, WIF).U)
        val vec_bias_fixed = vec_bias_float.map(f => floatToFixed(f, WII, WIF).U)

        // 2. 计算软件期望结果
        val expected_res_float = calculateLayerNorm(vec_x_float, vec_weight_float, vec_bias_float)
        val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF))

        println(s"Input vector x: ${vec_x_float.take(5).mkString(", ")}...")
        println(s"Weight vector: ${vec_weight_float.take(5).mkString(", ")}...")
        println(s"Bias vector: ${vec_bias_float.take(5).mkString(", ")}...")
        println(s"Expected result: ${expected_res_float.take(5).mkString(", ")}...")

        // 输入x
        fork {
          if (dut.io.x.ready.peekBoolean() && dut.io.weight.ready.peekBoolean() && dut.io.bias.ready.peekBoolean()) {
            dut.io.x.valid.poke(true.B)
            dut.io.weight.valid.poke(true.B)
            dut.io.bias.valid.poke(true.B)
            for (i <- 0 until VECTOR_SIZE) {
              dut.io.x.bits(i).poke(vec_x_fixed(i))
              dut.io.weight.bits(i).poke(vec_weight_fixed(i))
              dut.io.bias.bits(i).poke(vec_bias_fixed(i))
            }
          } else {
            dut.io.x.valid.poke(false.B)
            dut.io.weight.valid.poke(false.B)
            dut.io.bias.valid.poke(false.B)
          }
          dut.clock.step(1)

        }.fork {
          // 输出检查
          var cycle_count = 0
          val max_cycles = 1000 // 设置最大周期数防止无限循环
          while (!dut.io.normRes.valid.peekBoolean() && cycle_count < max_cycles) {
            dut.clock.step(1)
            cycle_count += 1
          }
          dut.io.normRes.ready.poke(true.B)
          // 验证结果
          var all_correct = true
          for (i <- 0 until VECTOR_SIZE) {
            val expected_val = expected_res_fixed(i)
            val actual_val = dut.io.normRes.bits(i).peek().litValue
            val tolerance = BigInt(1) << (WIF - 2) // 允许一定的误差
            val expected_float = fixedToFloat(expected_val, WII, WIF)
            val actual_float = fixedToFloat(actual_val, WII, WIF)
            println(
              s"Index $i: Expected $expected_float (${expected_val}), Got $actual_float (${actual_val})"
            )
            val diff = (expected_val - actual_val).abs
            if (diff > tolerance) {
              println(
                s"Index $i: Expected $expected_float (${expected_val}), Got $actual_float (${actual_val}), Diff: ${diff}"
              )
              all_correct = false
            }
          }

          if (all_correct) {
            println(s"LayerNorm test passed! All ${VECTOR_SIZE} elements match within tolerance.")
            println(s"Total cycles taken: $cycle_count")
          } else {
            println(s"LayerNorm test failed! Some elements don't match within tolerance.")
          }

          if (cycle_count >= max_cycles) {
            println(s"Test timeout after $max_cycles cycles!")
          }
        }.join()
      }
  }

  // it should "handle edge cases correctly" in {
  //   val WII = 8
  //   val WIF = 8
  //   val VECTOR_SIZE = 16
  //   val NUM_PE = 4

  //   test(new LayerNorm(WII, WIF, VECTOR_SIZE, NUM_PE))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       // 测试边界情况：所有输入值相同
  //       val constant_value = 2.0f
  //       val vec_x_float = Seq.fill(VECTOR_SIZE)(constant_value)
  //       val vec_weight_float = Seq.fill(VECTOR_SIZE)(1.0f)
  //       val vec_bias_float = Seq.fill(VECTOR_SIZE)(0.0f)

  //       val vec_x_fixed = vec_x_float.map(f => floatToFixed(f, WII, WIF).U)
  //       val vec_weight_fixed = vec_weight_float.map(f => floatToFixed(f, WII, WIF).U)
  //       val vec_bias_fixed = vec_bias_float.map(f => floatToFixed(f, WII, WIF).U)

  //       // 计算期望结果
  //       val expected_res_float = calculateLayerNorm(vec_x_float, vec_weight_float, vec_bias_float)
  //       val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF))

  //       println(s"Testing edge case: constant input value $constant_value")
  //       println(s"Expected normalized result: ${expected_res_float.take(5).mkString(", ")}...")

  //       dut.reset.poke(true.B)
  //       dut.clock.step(1)
  //       dut.reset.poke(false.B)

  //       // 输入数据
  //       fork {
  //         dut.io.x.valid.poke(true.B)
  //         for (i <- 0 until VECTOR_SIZE) {
  //           dut.io.x.bits(i).poke(vec_x_fixed(i))
  //         }
  //         dut.clock.step(1)
  //         dut.io.x.valid.poke(false.B)
  //       }.fork {
  //         dut.io.weight.valid.poke(true.B)
  //         for (i <- 0 until VECTOR_SIZE) {
  //           dut.io.weight.bits(i).poke(vec_weight_fixed(i))
  //         }
  //         dut.clock.step(1)
  //         dut.io.weight.valid.poke(false.B)
  //       }.fork {
  //         dut.io.bias.valid.poke(true.B)
  //         for (i <- 0 until VECTOR_SIZE) {
  //           dut.io.bias.bits(i).poke(vec_bias_fixed(i))
  //         }
  //         dut.clock.step(1)
  //         dut.io.bias.valid.poke(false.B)
  //       }.fork {
  //         // 等待输出并验证
  //         var received = false
  //         var cycle_count = 0
  //         val max_cycles = 500

  //         while (!received && cycle_count < max_cycles) {
  //           if (dut.io.normRes.valid.peekBoolean()) {
  //             dut.io.normRes.ready.poke(true.B)

  //             // 对于常数输入，归一化后应该接近0（因为标准差接近0）
  //             var all_correct = true
  //             for (i <- 0 until VECTOR_SIZE) {
  //               val actual_val = dut.io.normRes.bits(i).peek().litValue
  //               val actual_float = fixedToFloat(actual_val, WII, WIF)

  //               // 对于常数输入，结果应该接近偏置值
  //               val expected_float = vec_bias_float(i)
  //               val tolerance = 0.1f

  //               if (math.abs(actual_float - expected_float) > tolerance) {
  //                 println(s"Index $i: Expected ~$expected_float, Got $actual_float")
  //                 all_correct = false
  //               }
  //             }

  //             if (all_correct) {
  //               println(s"Edge case test passed! Constant input handled correctly.")
  //             } else {
  //               println(s"Edge case test failed!")
  //             }

  //             received = true
  //           } else {
  //             dut.io.normRes.ready.poke(false.B)
  //             dut.clock.step(1)
  //             cycle_count += 1
  //           }
  //         }

  //         if (cycle_count >= max_cycles) {
  //           println(s"Edge case test timeout after $max_cycles cycles!")
  //         }
  //       }.join()
  //     }
  // }

}

object VerilogGen extends App {
  (new ChiselStage).execute(
    Array("-td", "verilog", "--target", "verilog"),
    Seq(ChiselGeneratorAnnotation(() => new LayerNorm(WII = 8, WIF = 8, VECTOR_SIZE = 4096, NUM_PE = 64)))
  )
}
