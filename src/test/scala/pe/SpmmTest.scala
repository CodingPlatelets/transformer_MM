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
  behavior.of("tester on sddmm")
  it should "sddmm should calculate in lines" in {
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
          dut.io.vMatrix(i)(m).poke(testV(i)(m).U)
        }
      }
      dut.clock.step()

      fork {
        var cnt = 0
        while (cnt < numOfMask) {
          if (dut.io.inMask.ready.peekBoolean() && dut.io.nums.ready.peekBoolean()) {
            for (i <- 0 until L) {
              dut.io.nums.bits(i).poke(testNum(i).U)
            }
            for (i <- 0 until numOfMask) {
              dut.io.inMask.bits(i).poke(mask(cnt)(i).U)
            }

            parallel(
              dut.io.inMask.valid.poke(true.B),
              dut.io.nums.valid.poke(true.B)
            )
            cnt = cnt + 1
          }
          dut.clock.step()
        }
        dut.io.inMask.valid.poke(false.B)
        dut.io.nums.valid.poke(false.B)

      }.fork {
        var cnt = 0
        while (cnt < numOfMask) {
          dut.io.res.ready.poke(false.B)
          if (dut.io.res.valid.peekBoolean()) {
            for (i <- 0 until dimV) {
              dut.io.res.bits(i).expect(res(cnt)(i).U)
            }
            dut.io.outMask.valid.expect(true.B)
            for (i <- 0 until numOfMask) {
              dut.io.outMask.bits(i).expect(mask(cnt)(i).U)
            }
            parallel(
              dut.io.res.ready.poke(true.B),
              dut.io.outMask.ready.poke(true.B)
            )
            cnt = cnt + 1
          }
          dut.clock.step()
        }
        dut.io.res.ready.poke(false.B)
        dut.io.outMask.ready.poke(false.B)
      }.join()

    }
  }

}
