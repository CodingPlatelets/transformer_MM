package kernel.deprecated
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
class SddmmTest extends AnyFlatSpec with ChiselScalatestTester {
  val bit = 32
  val dimV = 18
  val L = 28
  val numOfMask = 4
  val queueSize = 2
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on sddmm")
  it should "sddmm should calculate in lines" taggedAs deprecated in {
    test(new Sddmm(bit, dimV, L, numOfMask, queueSize)).withAnnotations(annos) { dut =>
      val testQ = for (i <- 0 until dimV) yield (i * 2)
      val mask1 = Seq(0, 2, 4, 7)
      val mask2 = Seq(1, 3, 5, 6)
      val mask3 = Seq(3, 4, 6, 7)
      val mask = Seq(mask1, mask2, mask3)

      val testK =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j)

      val res1 = Array.fill(L)(0)
      val res2 = Array.fill(L)(0)
      val res3 = Array.fill(L)(0)
      for (i <- 0 until numOfMask) {
        res1(mask1(i)) = testQ.zip(testK(mask1(i))).map { case (a, b) => a * b }.sum
        res2(mask2(i)) = testQ.zip(testK(mask2(i))).map { case (a, b) => a * b }.sum
        res3(mask3(i)) = testQ.zip(testK(mask3(i))).map { case (a, b) => a * b }.sum
      }

      val res = Seq(res1, res2, res3)

      println(res1.toIndexedSeq)
      println(res2.toIndexedSeq)
      println(res3.toIndexedSeq)

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.kMatrix(j)(i).poke(testK(j)(i).U)
        }
      }
      dut.clock.step()

      fork {
        var cnt = 0
        while (cnt < mask.length) {
          if (dut.InputPipe.ready.peekBoolean() && dut.InputPipe.ready.peekBoolean()) {
            for (i <- 0 until numOfMask) {
              dut.InputPipe.bits.mask(i).poke(mask(cnt)(i).U)
            }
            for (i <- 0 until dimV) {
              dut.InputPipe.bits.value(i).poke(testQ(i).U)
            }
            dut.InputPipe.valid.poke(true.B)
            cnt += 1
          }
          dut.clock.step()
        }
        dut.InputPipe.valid.poke(false.B)
      }.fork {
        var cntR = 0
        while (cntR < mask.length) {

          if (dut.OutputPipe.valid.peekBoolean()) {
            for (i <- 0 until numOfMask) {
              dut.OutputPipe.bits.value(mask(cntR)(i)).expect(res(cntR)(mask(cntR)(i)).U)
              dut.OutputPipe.bits.mask(i).expect(mask(cntR)(i).U)
            }
            dut.OutputPipe.ready.poke(true.B)

            cntR += 1
          } else {
            dut.OutputPipe.ready.poke(false.B)
          }

          dut.clock.step()
        }

        dut.OutputPipe.ready.poke(false.B)
      }.join()

    }
  }
}
