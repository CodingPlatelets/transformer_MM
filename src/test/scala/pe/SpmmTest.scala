package pe
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
class SpmmTest extends AnyFlatSpec with ChiselScalatestTester {

  val bit = 16
  val dimV = 8
  val L = 28
  val numOfMask = 3
  val queueSize = 10
  behavior.of("tester on spmm")
  it should "spmm should calculate in lines" in {
    test(new SpMM(bit, dimV, L, 1, numOfMask, queueSize)) { dut =>
      var mask1 = Seq(1, 2, 3)
      var mask2 = Seq(1, 4, 6)
      var mask3 = Seq(2, 3, 7)
      var mask = Seq(mask1, mask2, mask3)

      val testV = for (i <- 0 until L) yield for (j <- 0 until dimV) yield (i + j)
      val testNum = for (i <- 1 to L) yield (i * 2)

      var res01 = Array.fill(dimV)(0)
      var res02 = Array.fill(dimV)(0)
      var res03 = Array.fill(dimV)(0)

      for (i <- 0 until numOfMask) {
        for (j <- 0 until dimV) {
          res01(j) = res01(j) + testNum(mask1(i)) * testV(mask1(i))(j)
          res02(j) = res02(j) + testNum(mask2(i)) * testV(mask2(i))(j)
          res03(j) = res03(j) + testNum(mask3(i)) * testV(mask3(i))(j)
        }
      }
      val res = Seq(res01, res02, res03)

      println("mask0 is " + mask)
      println("vMatrix is " + testV)
      println("nums is " + testNum)
      println("res01 is " + res01.toIndexedSeq)
      println("res02 is " + res02.toIndexedSeq)
      println("res03 is " + res03.toIndexedSeq)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      for (i <- 0 until L) {
        for (m <- 0 until dimV) {
          dut.vMatrix(i)(m).poke(testV(i)(m).U)
        }
      }
      dut.clock.step()

      fork {
        var cnt = 0
        while (cnt < numOfMask) {
          if (dut.InputPipe.ready.peekBoolean()) {
            for (i <- 0 until L) {
              dut.InputPipe.bits.value(i).poke(testNum(i).U)
            }
            for (i <- 0 until numOfMask) {
              dut.InputPipe.bits.mask(i).poke(mask(cnt)(i).U)
            }
            dut.InputPipe.valid.poke(true.B)
            cnt = cnt + 1
          }
          dut.clock.step()
        }
        dut.InputPipe.valid.poke(false.B)
      }.fork {
        var cnt = 0
        while (cnt < numOfMask) {
          dut.OutputPipe.ready.poke(false.B)
          if (dut.OutputPipe.valid.peekBoolean()) {
            for (i <- 0 until dimV) {
              dut.OutputPipe.bits.value(i).expect(res(cnt)(i).U)
            }
            for (i <- 0 until numOfMask) {
              dut.OutputPipe.bits.mask(i).expect(mask(cnt)(i).U)
            }
            dut.OutputPipe.ready.poke(true.B)
            cnt = cnt + 1
          }
          dut.clock.step()
        }
        dut.OutputPipe.ready.poke(false.B)
      }.join()

    }
  }

}
