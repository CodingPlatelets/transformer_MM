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

  behavior.of("tester on Float2Fxp zoom")
  it should "convert float to fixed point correctly" in {
    test(new Float2Fxp).withAnnotations(annos) { dut =>
      fork {
        // 测试正数转换
        dut.io.in.poke("h3f800000".U) // 1.0f
        dut.clock.step()

        // 测试负数转换
        dut.io.in.poke("hbf800000".U) // -1.0f
        dut.clock.step()

        // 测试溢出情况
        dut.io.in.poke("h7f7fffff".U) // 最大正数
        dut.clock.step()
        dut.io.in.poke("hff7fffff".U) // 最小负数
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("h0100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("hff00".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("h7fff".U)
        dut.io.overflow.expect(true.B)

        dut.clock.step()
        dut.io.out.expect("h8000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }

  behavior.of("tester on FxpZoom")
  it should "zoom fixedpoint correctly" in {
    test(new FxpZoom(8, 8, 7, 5, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      fork {
        // Test case 1: Carrier case
        dut.io.in.poke("b00000000_11111111".U) // 8-bit integer part and 8-bit fractional part
        dut.clock.step()

        // Test case 2: Round case
        dut.io.in.poke("b11111111_11111111".U) // Maximum value
        dut.clock.step()

        // Test case 3: normal case
        dut.io.in.poke("b00000000_10000001".U) // Value that should cause rounding
        dut.clock.step()

        // Test case 4: overflow
        dut.io.in.poke("b10000000_10000000".U) // Negative value
        dut.clock.step()
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("b0000001_00000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b000000_00000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b0000000_10000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b1000000_00000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }

  behavior.of("FxpAddSub")
  it should ("do add and sub correctly") in {
    test(new FxpAddSub(8, 8, 6, 5, 7, 7, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      fork {
        // Test case 1: Carrier case
        dut.io.ina.poke("b00000000_11111111".U)
        dut.io.inb.poke("b000000_11111".U)
        dut.io.sub.poke(common.AddOrSub.ADD)
        dut.clock.step()

        // Test case 2: Round case
        dut.io.ina.poke("b11111111_11111111".U) // Maximum value
        dut.io.inb.poke("b111111_11111".U) // Maximum value
        dut.io.sub.poke(common.AddOrSub.SUB)
        dut.clock.step()

        // Test case 3: overflow
        dut.io.ina.poke("b10000000_10000000".U) // Negative value
        dut.io.inb.poke("b100100_10000".U) // Negative value
        dut.io.sub.poke(common.AddOrSub.ADD)
        dut.clock.step()
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("b0000001_1111100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b0000000_0000100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b1000000_0000000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }
}