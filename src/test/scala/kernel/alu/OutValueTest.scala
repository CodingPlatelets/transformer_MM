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

    val Scores = matInit[T](m, m)
    val Value = matInit[T](m, n)
    val expectedResults = mmul(Scores, Value)
    
    printmat(Scores)
    printmat(Value)
    printmat(expectedResults)

    if (dut.io.Scores.ready.peekBoolean() && dut.io.Value.ready.peekBoolean()) {
      println("Scores and Value are ready")
      dut.io.Scores.valid.poke(true.B)
      dut.io.Value.valid.poke(true.B)
      
      for {
        row <- 0 until m
        col <- 0 until n
        i <- 0 until m
      } {
        dut.io.Scores.bits(row)(i).poke(toBinaryBigInt(Scores(row)(i)).U)
        dut.io.Value.bits(i)(col).poke(toBinaryBigInt(Value(i)(col)).U)
      }
    } else {
      dut.io.Scores.valid.poke(false.B)
      dut.io.Value.valid.poke(false.B)
    }

    while (!dut.io.AttnOut.valid.peekBoolean()) {
      dut.clock.step()
    }

    dut.io.AttnOut.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0
    
    for {
      row <- 0 until m
      col <- 0 until n
    } {
      val outBigInt = dut.io.AttnOut.bits(row)(col).peekInt()
      val out = fromBinaryBigInt[T](outBigInt)
      val expected = expectedResults(row)(col)
      checkResult(out, expected, row, col, precision) match {
        case Some(_) => invalidcnt += 1
        case None    => // right
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testOutValueSingle[T: Numeric: ClassTag](
    dut: OutValueSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    val AttnWeights = matInit[T](m, m)
    val Value = matInit[T](m, n)
    val expectedResults = mmul(AttnWeights, Value)
    
    printmat(AttnWeights)
    printmat(Value)
    printmat(expectedResults)

    val precision = 0.001f
    var invalidcnt = 0

    if (dut.io.Value.ready.peekBoolean()) {
      println("Value is ready")
      dut.io.Value.valid.poke(true.B)
      
      for {
        i <- 0 until m
        j <- 0 until n
      } {
        dut.io.Value.bits(i)(j).poke(toBinaryBigInt(Value(i)(j)).U)
      }
    } else {
      dut.io.Value.valid.poke(false.B)
    }

    for (index <- 0 until m) {
      if (dut.io.currentScores.ready.peekBoolean()) {
        println(s"currentScores index: $index is ready")
        dut.io.currentScores.valid.poke(true.B)
        
        for (i <- 0 until m) {
          dut.io.currentScores.bits.value(i).poke(toBinaryBigInt(AttnWeights(index)(i)).U)
        }
      } else {
        dut.io.currentScores.valid.poke(false.B)
      }
      
      dut.io.currentAttnOut.ready.poke(false.B)
      while (!dut.io.currentAttnOut.valid.peekBoolean()) {
        dut.clock.step()
      }
      
      dut.io.currentAttnOut.ready.poke(true.B)
      val currentRowIndex = dut.io.currentAttnOut.bits.index.peekInt()
      
      for (i <- 0 until n) {
        val outBigInt = dut.io.currentAttnOut.bits.value(i).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(currentRowIndex.toInt)(i)
        
        checkResult(out, expected, currentRowIndex.toInt, i, precision) match {
          case Some(_) => invalidcnt += 1
          case None    => // right
        }
      }
      dut.clock.step()
    }

    dut.io.done.expect(true.B)

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  "OutValueSingle " should "compute fxp matrix multiplication" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new OutValueSingle(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        testOutValueSingle[Int](dut)
      }
  }

  // "OutValue " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new OutValue(m = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testOutValue[Int](dut)
  //     }
  // }
}
