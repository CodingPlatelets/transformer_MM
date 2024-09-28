package pe
import chisel3._
import chiseltest._
import pe.utils._
import org.scalatest.flatspec.AnyFlatSpec

class FXPArithmTest extends AnyFlatSpec with ChiselScalatestTester {

  val bit = 64
  val dimV = 32
  val depth = 128
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fifo with memory")
  it should "fifo with it" in {
    test(new Float2FxpPipe)
      .withAnnotations(annos) { dut => }
  }
}

class Float2FxpTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "Float2Fxp"

  it should "convert float to fixed point correctly" in {
    test(new Float2Fxp) { dut =>
      // 测试正数转换
      dut.io.in.poke("h3f800000".U) // 1.0f
      dut.clock.step(2)
      dut.io.out.expect("h0100".U)
      dut.io.overflow.expect(false.B)

      // 测试负数转换  
      dut.io.in.poke("hbf800000".U) // -1.0f
      dut.clock.step(2)
      dut.io.out.expect("hff00".U)
      dut.io.overflow.expect(false.B)

      // 测试溢出情况
      dut.io.in.poke("h7f7fffff".U) // 最大正数
      dut.clock.step(2)
      dut.io.out.expect("h7fff".U)
      dut.io.overflow.expect(true.B)

      dut.io.in.poke("hff7fffff".U) // 最小负数
      dut.clock.step(2)
      dut.io.out.expect("h8000".U)
      dut.io.overflow.expect(true.B)
    }
  }
}
