package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag
import kernel.alu.{DataWidthConfig, Fp32Config, Fp64Config, FxpConfig, GEMMDataType}
import ujson.Arr
import Utils._

class OutValueTest extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

  private def testOutValue[T: Numeric: ClassTag](
    dut: OutValue
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    var caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inScores = matInit[T](m, m)
      val inValue = matInit[T](m, n)
      val exAttnOut = mmul(inScores, inValue)
      (inScores, inValue, exAttnOut)
    }
    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inScores, inValue, _) = testCases(cnt)
        if (dut.io.Scores.ready.peekBoolean() && dut.io.Value.ready.peekBoolean()) {
          println("test case " + cnt + ": Scores and Value are ready")
          dut.io.Scores.valid.poke(true.B)
          dut.io.Value.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until m
          } {
            dut.io.Scores.bits(row)(i).poke(toBinaryBigInt(inScores(row)(i)).U)
            dut.io.Value.bits(i)(col).poke(toBinaryBigInt(inValue(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.Scores.valid.poke(false.B)
          dut.io.Value.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exAttnOut) = testCases(resCnt)
        val precision = 0.001f
        var invalidcnt = 0
        if (dut.io.AttnOut.valid.peekBoolean()) {
          dut.io.AttnOut.ready.poke(true.B)
          val (_, _, exAttnOut) = testCases(resCnt)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            val outBigInt = dut.io.AttnOut.bits(row)(col).peekInt()
            val out = fromBinaryBigInt[T](outBigInt)
            val expected = exAttnOut(row)(col)
            checkResult(out, expected, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println("case " + resCnt + ": Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.AttnOut.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()
  }

  private def testOutValueSingle[T: Numeric: ClassTag](
    dut: OutValueSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    var caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inScores = matInit[T](m, m)
      val inValue = matInit[T](m, n)
      val exAttnOut = mmul(inScores, inValue)
      (inScores, inValue, exAttnOut)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inScores, inValue, _) = testCases(cnt)
        if (dut.io.Value.ready.peekBoolean()) {
          println("test case " + cnt + ": Value is ready")
          dut.io.Value.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            dut.io.Value.bits(row)(col).poke(toBinaryBigInt(inValue(row)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.Value.valid.poke(false.B)
        }
        var rowIdx = 0
        while (rowIdx < m) {
          if (dut.io.curScores.ready.peekBoolean()) {
            println(s"case ${cnt-1} curScores index: $rowIdx is ready")
            dut.io.curScores.valid.poke(true.B)
            for (i <- 0 until m) {
              dut.io.curScores.bits.value(i).poke(toBinaryBigInt(inScores(rowIdx)(i)).U)
            }
            dut.io.curScores.bits.index.poke(rowIdx.U)
            rowIdx += 1
          } else {
            dut.io.curScores.valid.poke(false.B)
          }
          dut.clock.step()
        }
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exAttnOut) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curAttnOut.valid.peekBoolean()) {
            dut.io.curAttnOut.ready.poke(true.B)
            val curRowIndex = dut.io.curAttnOut.bits.index.peekInt()
            dut.io.curAttnOut.bits.index.expect(rowIdx.U)
            for (i <- 0 until n) {
              val outBigInt = dut.io.curAttnOut.bits.value(i).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exAttnOut(rowIdx)(i)
              val precision = 0.001f
              checkResult(out, expected, rowIdx, i, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curAttnOut.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.clock.step()
        dut.io.done.expect(true.B)
        println(s"case $resCnt done\n")
        resCnt += 1
      }
    }.join()
  }

  //TODO: Fix the warning in the OutValueSingle module
  // warning: This module has an additional loop for the curScores input. 
  // The error might be caused by the valid signal of the input"curScores", but it hasn't been resolved yet.
  "OutValueSingle " should "compute fxp matrix multiplication" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new OutValueSingle(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        testOutValueSingle[Int](dut)
      }
  }

  // "OutValue " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new OutValue(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testOutValue[Int](dut)
  //     }
  // }
}
