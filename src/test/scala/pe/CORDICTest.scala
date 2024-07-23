package pe

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import fixedpoint._
import java.io._
import pe.configs.SdpmmConfigs
import os.write

class CORDICTest extends AnyFlatSpec with ChiselScalatestTester {

//   val bit = 64
//   val dimV = 32
//   val depth = 128
  val annos = Seq(VerilatorBackendAnnotation)
  val wholeWidth:      Int = 16
  val fractionalWidth: Int = 15
  val pow2 = scala.math.pow(2, fractionalWidth)
  behavior.of("tester on exp function in chisel")
  it should "exp in fixedpoint" in {
    test(new FixedPointExp(wholeWidth, fractionalWidth))
      .withAnnotations(annos) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
        val start_val = -0.5
        val end_val = 0.5
        val num = 500
        val writer = new PrintWriter(new File("test_results.csv"))

        writer.write("Input Value,Computed Exp,Actual Exp,Relative Error (%)\n")

        val start_iter = (start_val * num).toInt
        val end_iter = (end_val * num).toInt

        for (value <- start_iter until end_iter) {
          val floatValue = value.toFloat / num
          dut.io.x.poke(floatValue.F(fractionalWidth.BP).asSInt)
          dut.clock.step(1)
          val actualValue = scala.math.exp(floatValue)
          val computedValue = dut.io.exp_x.peek().litValue.toFloat / pow2
          val relativeError = ((actualValue - computedValue) / actualValue).abs * 100
          // assert(relativeError < 1)

          writer.write(f"$floatValue%.2f,$computedValue%.5f,$actualValue%.5f,$relativeError%.2f\n")
        }

        writer.close()
      }
  }

  it should "softmax in chisel3" in {
    test(new Softmax)
      .withAnnotations(annos) { dut =>
        val numOfMask = SdpmmConfigs.numOfMask
        val testQ = Seq.tabulate(SdpmmConfigs.dim)(x => scala.util.Random.nextInt(10) + 1)
        val inputTimes = 1

        val pow2 = scala.math.pow(2, SdpmmConfigs.bit - 1)

        val mask = for (i <- 0 until inputTimes) yield {
          Seq.fill(2 * numOfMask)(scala.util.Random.nextInt(SdpmmConfigs.L)).distinct.take(numOfMask)
        }
        var resultSoftmax = Seq.tabulate(SdpmmConfigs.dim) { i =>
          val exp = scala.math.exp(testQ(i))
          exp / (testQ.map(x => scala.math.exp(x)).sum)
        }
        println(mask)
        val writer = new PrintWriter(new File("softmax_test_results.csv"))

        writer.write("Input Value,Computed softmax,Actual softmax,Relative Error (%)\n")

        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        dut.clock.step()
        fork {
          var cnt = 0
          while (cnt < inputTimes) {
            if (dut.InputPipe.ready.peekBoolean()) {
              dut.InputPipe.valid.poke(true.B)
              for (i <- 0 until numOfMask) {
                dut.InputPipe.bits.mask(i).poke(mask(cnt)(i).U)
              }

              for (i <- 0 until SdpmmConfigs.dim) {
                dut.InputPipe.bits.value(i).poke(testQ(i).U)
              }
              cnt = cnt + 1
            } else {
              dut.InputPipe.valid.poke(false.B)
            }
            dut.clock.step()
          }

          dut.InputPipe.valid.poke(false.B)
        }.fork {
          var cntR = 0
          while (cntR < inputTimes) {
            if (dut.OutputPipe.valid.peekBoolean()) {
              for (i <- 0 until numOfMask) {
                dut.OutputPipe.bits.mask(i).expect(mask(cntR)(i))
              }
              for (i <- 0 until SdpmmConfigs.dim) {
                val com = dut.OutputPipe.bits.value(i).peek().litValue.toFloat / pow2
                val act = resultSoftmax(i)
                val relativeError = ((act - com) / act).abs * 100
                writer.write(f"${testQ(i)}%.2f,$com%.5f,$act%.5f,$relativeError%.2f\n")
              }
              dut.OutputPipe.ready.poke(true.B)
              cntR = cntR + 1
            } else {
              dut.OutputPipe.ready.poke(false.B)
            }
            dut.clock.step()
          }
          dut.OutputPipe.ready.poke(false.B)
        }.join()
        writer.close()
      }
  }
}
