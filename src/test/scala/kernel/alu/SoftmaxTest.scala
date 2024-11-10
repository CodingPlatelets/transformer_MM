package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import fixedpoint._
import java.io._
import kernel.configs.SdpmmConfigs
import os.write

class SoftmaxTest extends AnyFlatSpec with SoftmaxAccuracy with ChiselScalatestTester {

//   val bit = 64
//   val dimV = 32
//   val depth = 128
  val FF = 24
  val annos = Seq(VerilatorBackendAnnotation)
  val pow2 = scala.math.pow(2, FF).toFloat
  behavior.of("tester on exp function in chisel")
  it should "exp in fixedpoint" in {
    test(new FixedPointExp)
      .withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
        //generate a range number from -10.5 to 0.0 step 0.5
        val range = BigDecimal(-7.0) to BigDecimal(0.0) by BigDecimal(0.5)

        // val writer = new PrintWriter(new File("test_results.csv"))

        // writer.write("Input Value,Computed Exp,Actual Exp,Relative Error (%)\n")

        dut.io.x.valid.poke(false.B)

        fork {
          for (value <- range) {
            dut.io.x.valid.poke(true.B)
            dut.io.x.bits.poke(value.F(F.BP).asSInt)
            dut.clock.step()
          }

          dut.io.x.valid.poke(false.B)
        }.fork {
          dut.clock.step(dut.expDelay)
          for (value <- range) {
            val actualValue = scala.math.exp(value.toDouble).toFloat
            dut.io.exp_x.valid.expect(true.B)
            val computedValue = dut.io.exp_x.bits.peekInt().toFloat / pow2
            val relativeError = ((actualValue - computedValue) / actualValue).abs * 100

            // println(
            //   s"actualValue is $actualValue,\t computedValue is $computedValue,\t relativeError is $relativeError"
            // )
            assert(relativeError < 5)

            dut.clock.step()
          }
          dut.io.exp_x.valid.expect(false.B)
        }.join()

      // writer.close()
      }
  }
  def doubleToFixedPoint(d: Float, intBits: Int, fracBits: Int): BigInt = {
    // 检查数值范围
    val maxVal = Math.pow(2, intBits - 1) - Math.pow(2, -fracBits)
    val minVal = -Math.pow(2, intBits - 1)
    require(d <= maxVal && d >= minVal, s"Value $d out of range [$minVal, $maxVal]")

    // 转换为定点数表示
    BigInt((d * (1L << fracBits)).round)
  }

  val arraySize = 4
  it should "pass softmax test" in {
    test(new Softmax(arraySize))
      .withAnnotations(annos) { dut =>
        val rseed = 4
        val rnd = new scala.util.Random(rseed)
        val testQ = Seq.tabulate(arraySize)(_ => rnd.nextFloat())

        val maxInQ = testQ.max
        val expInQ = testQ.map(x => scala.math.exp(x - maxInQ))
        val sumExpInQ = expInQ.sum
        val resultSoftmax = expInQ.map(_ / sumExpInQ)

        println(s"testQ: ${testQ}")
        println(s"resultSoftmax: ${resultSoftmax}")
        println(s"maxInResult: ${resultSoftmax.max}")

        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()

        fork {
          dut.io.x.valid.poke(true.B)
          for (i <- 0 until arraySize) {
            println(s"testQ($i): ${doubleToFixedPoint(testQ(i), I, F)}")
            dut.io.x.bits(i).poke(doubleToFixedPoint(testQ(i), I, F).U)
          }
          dut.clock.step()
          dut.io.x.valid.poke(false.B)
        }.fork {
          var clk = 0
          while (!dut.io.soft_x.valid.peekBoolean()) {
            dut.clock.step()
            clk += 1
          }
          for (i <- 0 until arraySize) {
            val computedValue = dut.io.soft_x.bits(i).peekInt().toFloat / pow2
            val relativeError = ((resultSoftmax(i) - computedValue) / resultSoftmax(i)).abs * 100
            println(
              s"actualValue is ${resultSoftmax(i)},\t computedValue is $computedValue,\t relativeError is $relativeError"
            )
            // assert(relativeError < 5)
          }
          println(s"clk: $clk")
          dut.clock.step()
          dut.io.soft_x.valid.expect(false.B)
        }.join()

      }
  }
}
