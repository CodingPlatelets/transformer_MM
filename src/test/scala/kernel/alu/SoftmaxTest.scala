package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import fixedpoint._
import java.io._
import kernel.configs.SdpmmConfigs
import os.write
import java.lang.Float

class FixedPointExpTest extends AnyFlatSpec with SoftmaxAccuracy with ChiselScalatestTester {

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
        dut.clock.step()
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
}




class MultiPEFxpExpTest extends AnyFlatSpec with SoftmaxAccuracy with ChiselScalatestTester {
  def doubleToFixedPoint(d: Float, intBits: Int, fracBits: Int): BigInt = {
    // 检查数值范围
    val maxVal = Math.pow(2, intBits - 1) - Math.pow(2, -fracBits)
    val minVal = -Math.pow(2, intBits - 1)
    require(d <= maxVal && d >= minVal, s"Value $d out of range [$minVal, $maxVal]")

    // 转换为定点数表示
    BigInt((d * (1L << fracBits)).round)
  }
  val arraySize = 16
  val numPE = 4
  val FF = 24
  val pow2 = scala.math.pow(2, FF).toFloat
  val annos = Seq(VerilatorBackendAnnotation)
  it should "multi exp in fixedpoint" in {
    test(new MultiPEFxpExp( numPE))
      .withAnnotations(annos) { dut =>
        dut.clock.setTimeout(100) 
        val rseed = 4
        val rnd = new scala.util.Random(rseed)
        val testQ = Seq.tabulate(numPE)(_ => rnd.nextFloat())

        val maxInQ = testQ.max
        val expInQ = testQ.map(x => scala.math.exp(x - maxInQ))
        val sumExpInQ = expInQ.sum
        //val resultSoftmax = expInQ.map(_ / sumExpInQ)
        val resultExp = expInQ

        println(s"testQ: ${testQ}")
        println(s"resultExp: ${resultExp}")
        println(s"maxInResult: ${resultExp.max}")
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
      fork {
          dut.io.max.poke(doubleToFixedPoint(maxInQ, I, F).U)
          
          for (i <- 0 until numPE) {
            println(s"testQ($i): ${doubleToFixedPoint(testQ(i), I, F)}")
            dut.io.x.bits(i).poke(doubleToFixedPoint(testQ(i), I, F).U)
          }
          dut.io.x.valid.poke(true.B)
          dut.clock.step()
      }.fork {
         var clk = 0
          while (!dut.io.valid.peekBoolean()) {
            dut.clock.step()
            clk += 1
          }
        for (i <- 0 until numPE) {
          val computedValue =  dut.io.exp_x(i).peekInt().toFloat / pow2
          val relativeError = ((resultExp(i) - computedValue) / resultExp(i)).abs * 100
          println(
            s"actualValue is ${resultExp(i)},\t computedValue is $computedValue,\t relativeError is $relativeError"
          )
          // assert(relativeError < 5)
        }
      }.join()
  }
}
}


class SoftmaxTest extends AnyFlatSpec with SoftmaxAccuracy with ChiselScalatestTester {
    

  def doubleToFixedPoint(d: Float, intBits: Int, fracBits: Int): BigInt = {
    // 检查数值范围
    val maxVal = Math.pow(2, intBits - 1) - Math.pow(2, -fracBits)
    val minVal = -Math.pow(2, intBits - 1)
    require(d <= maxVal && d >= minVal, s"Value $d out of range [$minVal, $maxVal]")

    // 转换为定点数表示
    BigInt((d * (1L << fracBits)).round)
  }
  val arrayNum=4
  val arraySize = 16
  val numPE = 4
  val FF = 24
  val queueDepth = 100
  val pow2 = scala.math.pow(2, FF).toFloat
  val annos = Seq(VerilatorBackendAnnotation)
  it should "softmax in float" in {
    test(new Softmax1(arraySize, numPE, queueDepth))
      .withAnnotations(annos) { dut =>
        // dut.clock.setTimeout(1000) 
        val rseed = 4
        val rnd = new scala.util.Random(rseed)
        val testQ =Seq.tabulate(arrayNum,arraySize)((row,col) => rnd.nextFloat())

        val maxInQ = testQ.map(row=>row.max)
        val expInQ = testQ.zip(maxInQ).map { case (row, maxVal) =>
  row.map(x => scala.math.exp(x - maxVal))
}
        val sumExpInQ = expInQ.map(row=>row.sum)
        val resultSoftmax = expInQ.zip(sumExpInQ).map{case (row,sumVal)=>row.map(x => x / sumVal)}
        val row_in=0
        val row_out=0
        //val resultExp = expInQ

        println(s"testQ: ${testQ}")
        println(s"resultSoftmax: ${resultSoftmax}")
        println(s"maxInResult: ${resultSoftmax.map(row=>row.max)}")
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
      fork {
        for (row <- 0 until arrayNum) {
          dut.io.x.valid.poke(true.B)
          for (i <- 0 until arraySize) {
            println(s"testQ($row,$i): ${Float.floatToRawIntBits(testQ(row)(i))}") 
            dut.io.x.bits(i).poke(Float.floatToRawIntBits(testQ(row)(i)).U) 
          }
          dut.clock.step()
          dut.io.x.valid.poke(false.B)
        }
       
      }.fork {
        for (row <- 0 until arrayNum) {
          var clk = 0 
          while (!dut.io.soft_x.valid.peekBoolean()) {
            dut.clock.step()
            clk += 1
          }
          println(s"clk: $clk")
        for (i <- 0 until arraySize) {
          var computedValue = Seq.fill(arrayNum)(Seq.fill(arraySize)(0.0f))
          computedValue = computedValue.updated(row, computedValue(row).updated(i, dut.io.soft_x.bits(i).peekInt().toFloat / pow2))
          
          val relativeError = ((resultSoftmax(row)(i) - computedValue(row)(i)) / resultSoftmax(row)(i)).abs * 100
          println(
            s"row: $row, col: $i, actualValue is ${resultSoftmax(row)(i)},\t computedValue is ${computedValue(row)(i)},\t relativeError is $relativeError"
          )
         //assert(relativeError < 5)
        } 
        dut.clock.step()
        }
      }.join()
  }
}
}



