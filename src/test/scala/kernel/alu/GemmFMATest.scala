package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag
import kernel.alu.{DataWidthConfig, Fp32Config, Fp64Config, FxpConfig, GEMMDataType}
import ujson.Arr
import Utils._

class GEMMFMATest extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

  private def testMultiFMA[T: Numeric: ClassTag](
    dut: MultiFMA
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val k = dut.k
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    val matrixA_row = matInit[T](1, k)
    val matrixB_cols = matInit[T](k, peCount)
    val expectedResults = mmul(matrixA_row, matrixB_cols)
    
    printmat(matrixA_row)
    printmat(matrixB_cols)
    printmat(expectedResults)

    dut.io.reset.poke(true.B)
    dut.clock.step(1)
    dut.io.reset.poke(false.B)

    if (dut.io.matrixA_row.ready.peekBoolean() && dut.io.matrixB_cols.ready.peekBoolean()) {
      println("matrixA_row and matrixB_cols are ready")
      dut.io.matrixA_row.valid.poke(true.B)
      dut.io.matrixB_cols.valid.poke(true.B)
      
      for {
        i <- matrixA_row(0).indices
        j <- 0 until peCount
      } {
        dut.io.matrixA_row.bits(i).poke(toBinaryBigInt(matrixA_row(0)(i)).U)
        dut.io.matrixB_cols.bits(i)(j).poke(toBinaryBigInt(matrixB_cols(i)(j)).U)
      }
    } else {
      dut.io.matrixA_row.valid.poke(false.B)
      dut.io.matrixB_cols.valid.poke(false.B)
    }

    while (!dut.io.blockResult.valid.peekBoolean()) {
      dut.clock.step()
    }

    dut.io.blockResult.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0

    for (i <- 0 until peCount) {
      val outBigInt = dut.io.blockResult.bits(i).peekInt()
      val out = fromBinaryBigInt[T](outBigInt)
      val expected = expectedResults(0)(i)
      checkResult(out, expected, 0, i, precision) match {
        case Some(_) => invalidcnt += 1
        case None    => // right
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testGEMMFMATotal[T: Numeric: ClassTag](
    dut: GEMMFMATotal
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    val matrixA = matInit[T](m, k)
    val matrixB = matInit[T](k, n)
    val expectedResults = mmul(matrixA, matrixB)
    
    printmat(matrixA)
    printmat(matrixB)
    printmat(expectedResults)

    if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
      println("matrixA and matrixB are ready")
      dut.io.matrixA.valid.poke(true.B)
      dut.io.matrixB.valid.poke(true.B)
      
      for {
        row <- 0 until m
        col <- 0 until n
        i <- 0 until k
      } {
        dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
        dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
      }
    } else {
      dut.io.matrixA.valid.poke(false.B)
      dut.io.matrixB.valid.poke(false.B)
    }

    while (!dut.io.results.valid.peekBoolean()) {
      dut.clock.step()
    }

    dut.io.results.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0

    for {
      row <- 0 until m
      col <- 0 until n
    } {
      val outBigInt = dut.io.results.bits(row)(col).peekInt()
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

  private def testGEMMFMASingle[T: Numeric: ClassTag](
    dut: GEMMFMASingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    val matrixA = matInit[T](m, k)
    val matrixB = matInit[T](k, n)
    val expectedResults = mmul(matrixA, matrixB)
    
    printmat(matrixA)
    printmat(matrixB)
    printmat(expectedResults)

    if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
      println("matrixA and matrixB are ready")
      dut.io.matrixA.valid.poke(true.B)
      dut.io.matrixB.valid.poke(true.B)
      
      for {
        row <- 0 until m
        col <- 0 until n
        i <- 0 until k
      } {
        dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
        dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
      }
    } else {
      dut.io.matrixA.valid.poke(false.B)
      dut.io.matrixB.valid.poke(false.B)
    }

    val precision = 0.001f
    var invalidcnt = 0

    while (!dut.io.done.peekBoolean()) {
      if (dut.io.currentRow.valid.peekBoolean()) {
        val currentRowIndex = dut.io.currentRow.bits.index.peekInt()
        println(s"currentRow index: $currentRowIndex")
        
        for (i <- 0 until n) {
          val outBigInt = dut.io.currentRow.bits.value(i).peekInt()
          val out = fromBinaryBigInt[T](outBigInt)
          val expected = expectedResults(currentRowIndex.toInt)(i)
          
          checkResult(out, expected, currentRowIndex.toInt, i, precision) match {
            case Some(_) => invalidcnt += 1
            case None    => // right
          }
        }
      }
      dut.clock.step()
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testGEMMSingleQueue[T: Numeric: ClassTag](
    dut: GEMMSingleQueue
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    val matrixA = matInit[T](m, k)
    val matrixB = matInit[T](k, n)
    val expectedResults = mmul(matrixA, matrixB)
    
    printmat(matrixA)
    printmat(matrixB)
    printmat(expectedResults)

    if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
      println("matrixA and matrixB are ready")
      dut.io.matrixA.valid.poke(true.B)
      dut.io.matrixB.valid.poke(true.B)
      
      for {
        row <- 0 until m
        col <- 0 until n
        i <- 0 until k
      } {
        dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
        dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
      }
    } else {
      dut.io.matrixA.valid.poke(false.B)
      dut.io.matrixB.valid.poke(false.B)
    }

    dut.io.currentRow.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0

    while (!dut.io.done.peekBoolean()) {
      if (dut.io.currentRow.valid.peekBoolean()) {
        val currentRowIndex = dut.io.currentRow.bits.index.peekInt()
        println(s"currentRow index: $currentRowIndex")
        
        for (i <- 0 until n) {
          val outBigInt = dut.io.currentRow.bits.value(i).peekInt()
          val out = fromBinaryBigInt[T](outBigInt)
          val expected = expectedResults(currentRowIndex.toInt)(i)
          
          checkResult(out, expected, currentRowIndex.toInt, i, precision) match {
            case Some(_) => invalidcnt += 1
            case None    => // right
          }
        }
      }
      dut.clock.step()
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }


  "GEMMSingleQueue " should "compute fxp matrix multiplication" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new GEMMSingleQueue(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        testGEMMSingleQueue[Int](dut)
      }
  }

  // "GEMMSingleQueue " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new GEMMSingleQueue(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMSingleQueue[Float](dut)
  //     }
  // }

  // "GEMMFMATotal " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new GEMMFMATotal(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMFMATotal[Int](dut)
  //     }
  // }

  // "GEMMFMATotal " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new GEMMFMATotal(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMFMATotal[Float](dut)
  //     }
  // }

  // "GEMMFMASingle " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new GEMMFMASingle(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMFMASingle[Float](dut)
  //     }
  // }

  // "GEMMFMASingle " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new GEMMFMASingle(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMFMASingle[Int](dut)
  //     }
  // }

  // "MultiFMA " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new MultiFMA(k = 4, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testMultiFMA[Int](dut)
  //     }
  // }

  // "MultiFMA " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new MultiFMA( k = 4, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testMultiFMA[Float](dut)
  //     }
  // }
}
