package pe.utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class AverageModuleSpec extends AnyFlatSpec with ChiselScalatestTester {

  def log2Ceil(x: Int): Int = math.ceil(math.log(x) / math.log(2)).toInt

  behavior.of("AverageModule")

  it should "calculate the average correctly" in {
    test(new AverageModule(WII = 8, WIF = 8, WOI = 8, WOF = 8, ArraySize = 4))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        // 测试数据
        val testData = Seq(
          "00000100_00000000", // 4.0
          "00001000_00000000", // 8.0
          "00010000_00000000", // 12.0
          "00100000_00000000" // 16.0
        )

        val cyclesNeeded = log2Ceil(dut.ArraySize) * 2 + dut.WOI + dut.WOF + 5 + 1
        println(s"cyclesNeeded: $cyclesNeeded")

        fork {
          // 设置输入
          dut.io.in.valid.poke(true.B)
          testData.zipWithIndex.foreach {
            case (value, index) =>
              dut.io.in.bits(index).poke(("b" + value).U)
          }
          dut.clock.step(1)
          dut.io.in.valid.poke(false.B)
        }.fork {
          // 等待所需的周期数
          // dut.clock.step(cyclesNeeded)
          // 检查结果
          // dut.io.out.valid.expect(true.B)

          while (!dut.io.out.valid.peekBoolean()) {
            dut.clock.step(1)
          }
          dut.io.out.bits.expect("b0000_1010_0000_0000".U) // 10.0 in 8.8 fixed-point
        }.join()
      }
  }

  // it should "handle overflow correctly" in {
  //   test(new AverageModule(WII = 8, WIF = 8, WOI = 8, WOF = 8, ArraySize = 4))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //     // 测试数据 (故意设置大数以触发溢出)
  //     val testData = Seq.fill(4)(BigInt("7F00000000000000", 2)) // 127.0 (最大正数)

  //     val cyclesNeeded = log2Ceil(4)

  //     fork {
  //       // 设置输入
  //       dut.io.in.valid.poke(true.B)
  //       testData.zipWithIndex.foreach { case (value, index) =>
  //         dut.io.in.bits(index).poke(value.U)
  //       }
  //       dut.clock.step(1)
  //       dut.io.in.valid.poke(false.B)
  //     }.fork {
  //       // 等待所需的周期数
  //       dut.clock.step(cyclesNeeded)
  //       // 检查结果
  //       dut.io.out.valid.expect(true.B)
  //       dut.io.out.bits.expect(BigInt("FFFF", 16).U) // 预期溢出，返回最大值
  //     }.join()
  //   }
  // }

  // it should "handle multiple calculations correctly" in {
  //   test(new AverageModule(WII = 8, WIF = 8, WOI = 8, WOF = 8, ArraySize = 4))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //     // 定义多组测试数据
  //     val testSets = Seq(
  //       Seq(BigInt("0100000000000000", 2), BigInt("0200000000000000", 2), BigInt("0300000000000000", 2), BigInt("0400000000000000", 2)), // 平均值 10.0
  //       Seq(BigInt("0800000000000000", 2), BigInt("1000000000000000", 2), BigInt("1800000000000000", 2), BigInt("2000000000000000", 2))  // 平均值 40.0
  //     )

  //     val expectedResults = Seq(BigInt("0A00", 16), BigInt("2800", 16)) // 10.0 和 40.0 的 8.8 定点表示

  //     val cyclesNeeded = log2Ceil(4)

  //     fork {
  //       for (testSet <- testSets) {
  //         // 设置输入
  //         dut.io.in.valid.poke(true.B)
  //         testSet.zipWithIndex.foreach { case (value, index) =>
  //           dut.io.in.bits(index).poke(value.U)
  //         }
  //         dut.clock.step(1)
  //         dut.io.in.valid.poke(false.B)
  //         dut.clock.step(cyclesNeeded)
  //       }
  //     }.fork {
  //       for (expected <- expectedResults) {
  //         dut.clock.step(cyclesNeeded)
  //         dut.io.out.valid.expect(true.B)
  //         dut.io.out.bits.expect(expected.U)
  //         dut.clock.step(1)
  //       }
  //     }.join()
  //   }
  // }
}
