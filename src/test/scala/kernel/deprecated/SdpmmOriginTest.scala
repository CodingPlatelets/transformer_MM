package kernel.deprecated

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.stage.PrintFullStackTraceAnnotation

class SdpmmOriginTest extends AnyFlatSpec with ChiselScalatestTester {
  val bit = 64
  val dimV = 16
  // val L = 512
  val L = 128
  // val numOfMask = 8
  val numOfMask = 16
  val queueSize = 20
  val inPutTimes = L / 2
  val annos = Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)
  // val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on sdpmm origin")
  it should "sdpmmOrigin should calculate in lines" in {
    test(
      new SdpmmOrigin(
        bit,
        dimV,
        L,
        numOfMask,
        queueSize,
        "src/main/resources",
        inPutTimes
      )
    ).withAnnotations(annos).withChiselAnnotations(Seq(PrintFullStackTraceAnnotation)) { dut =>
      // sddmm
      val testQ = for (i <- 0 until dimV) yield (scala.util.Random.nextInt(10) + 1)

      val mask = for (i <- 0 until inPutTimes) yield {
        Seq.fill(2 * numOfMask)(scala.util.Random.nextInt(L)).distinct.take(numOfMask)
      }
      // val mask = Seq.tabulate(inPutTimes) { i =>
      //   Seq.tabulate(numOfMask)(j => (i * j) % numOfMask)
      // }
      // println(mask)

      val testK =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j)

      val testV =
        for (i <- 0 until L)
          yield for (j <- 0 until dimV) yield (i + j + 1)

      var temp = for (i <- 0 until inPutTimes) yield Array.fill(L)(BigInt(0))

      for (j <- 0 until inPutTimes) {
        for (i <- 0 until numOfMask) {
          temp(j)(mask(j)(i)) = testQ.zip(testK(mask(j)(i))).map { case (a, b) => a * b }.sum
        }
      }

      // spmm

      var res = for (i <- 0 until inPutTimes) yield Array.fill(dimV)(BigInt(0))

      for (m <- 0 until inPutTimes) {
        for (i <- 0 until numOfMask) {
          for (j <- 0 until dimV) {
            res(m)(j) = res(m)(j) + temp(m)(mask(m)(i)) * testV(mask(m)(i))(j)
          }
        }
      }

      // for (i <- 0 until inPutTimes) {
      //   println(s"res $i is ${res(i).toIndexedSeq}")
      // }

      @volatile var allClock = 0

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.kMatrix(j)(i).poke(testK(j)(i).U)
        }
      }

      for (j <- 0 until L) {
        for (i <- 0 until dimV) {
          dut.io.vMatrix(j)(i).poke(testV(j)(i).U)
        }
      }
      dut.clock.step()

      allClock = allClock + 1

      fork {
        var cnt = 0
        while (cnt < inPutTimes) {
          if (dut.InputPipe.ready.peekBoolean()) {
            for (i <- 0 until numOfMask) {
              dut.InputPipe.bits.mask(i).poke(mask(cnt)(i).U)
            }

            for (i <- 0 until dimV) {
              dut.InputPipe.bits.value(i).poke(testQ(i).U)
            }

            dut.InputPipe.valid.poke(true.B)
            cnt = cnt + 1
          } else {
            dut.InputPipe.valid.poke(false.B)
          }
          dut.clock.step()
          allClock = allClock + 1
        }

        dut.InputPipe.valid.poke(false.B)

      }.fork {
        var cntR = 0
        while (cntR < inPutTimes) {
          if (dut.OutputPipe.valid.peekBoolean()) {
            for (i <- 0 until dimV) {
              dut.OutputPipe.bits.value(i).expect(res(cntR)(i).U)
            }

            for (i <- 0 until numOfMask) {
              dut.OutputPipe.bits.mask(i).expect(mask(cntR)(i).U)
            }
            dut.OutputPipe.ready.poke(true.B)
            cntR = cntR + 1
          } else {
            dut.OutputPipe.ready.poke(false.B)
          }
          dut.clock.step()
          allClock = allClock + 1
        }

        dut.OutputPipe.ready.poke(false.B)
      }.join()

      println("Origin all clock is: " + allClock)
    }
  }
}
