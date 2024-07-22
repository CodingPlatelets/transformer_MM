package pe

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import fixedpoint._
import java.io._

class CORDICTest extends AnyFlatSpec with ChiselScalatestTester {

//   val bit = 64
//   val dimV = 32
//   val depth = 128
  val annos = Seq(VerilatorBackendAnnotation)
  val wholeWidth:      Int = 8
  val fractionalWidth: Int = 7
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
        val end_val = -0.002
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

          writer.write(f"$floatValue%.2f,$computedValue%.5f,$actualValue%.5f,$relativeError%.2f\n")
        }

        writer.close()
      }
  }
}
