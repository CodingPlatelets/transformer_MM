package kernel.alu

import chisel3._
import chiseltest._
import chisel3.util.DecoupledIO
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
  private def testMultiFMA_v2[T: Numeric: ClassTag](
    dut: MultiFMA_v2
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
    val gemmType = dut.gemmType

    val caseNum = 10

    val testCases = Array.tabulate(caseNum) { i =>
      val matrixA = matInit[T](m, k)
      val matrixB = matInit[T](k, n)
      val exResult = mmul(matrixA, matrixB)
      (matrixA, matrixB, exResult)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (matrixA, matrixB, _) = testCases(cnt)
        if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
          println(s"case $cnt : matrixA and matrixB are ready")
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
          cnt += 1
        } else {
          dut.io.matrixA.valid.poke(false.B)
          dut.io.matrixB.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exResult) = testCases(resCnt)
        if (dut.io.results.valid.peekBoolean()) {
          dut.io.results.ready.poke(true.B)
          val precision = 0.001f
          var invalidcnt = 0
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            val outBigInt = dut.io.results.bits(row)(col).peekInt()
            val out = fromBinaryBigInt[T](outBigInt)
            val expected = exResult(row)(col)
            checkResult(out, expected, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println(s"case $resCnt Verification passed!")
          else println(s"case $resCnt Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.results.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()
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

    val caseNum = 10

    val testCases = Array.tabulate(caseNum) { i =>
      val matrixA = matInit[T](m, k)
      val matrixB = matInit[T](k, n)
      val exResult = mmul(matrixA, matrixB)
      (matrixA, matrixB, exResult)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (matrixA, matrixB, _) = testCases(cnt)
        if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
          println(s"case $cnt : matrixA and matrixB are ready")
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
          cnt += 1
        } else {
          dut.io.matrixA.valid.poke(false.B)
          dut.io.matrixB.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exResult) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curRow.valid.peekBoolean()) {
            dut.io.curRow.ready.poke(true.B)
            val curRowIndex = dut.io.curRow.bits.index.peekInt()
            dut.io.curRow.bits.index.expect(rowIdx.U)
            println(s"curRow index: $curRowIndex")
            for (i <- 0 until n) {
              val outBigInt = dut.io.curRow.bits.value(i).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exResult(curRowIndex.toInt)(i)
              checkResult(out, expected, rowIdx, i, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curRow.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.io.done.expect(true.B)
        resCnt += 1
      }
    }.join()
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

    val caseNum = 10

    val testCases = Array.tabulate(caseNum) { i =>
      val matrixA = matInit[T](m, k)
      val matrixB = matInit[T](k, n)
      val exResult = mmul(matrixA, matrixB)
      (matrixA, matrixB, exResult)
    }
    fork {
      var cnt = 0
      while (cnt < caseNum) {
        dut.io.flush.poke(true.B)
        dut.clock.step()
        dut.io.flush.poke(false.B)
        val (matrixA, matrixB, _) = testCases(cnt)
        if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
          println(s"case $cnt : matrixA and matrixB are ready")
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
          cnt += 1
        } else {
          dut.io.matrixA.valid.poke(false.B)
          dut.io.matrixB.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exResult) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curRow.valid.peekBoolean()) {
            dut.io.curRow.ready.poke(true.B)
            val curRowIndex = dut.io.curRow.bits.index.peekInt()
            dut.io.curRow.bits.index.expect(rowIdx.U)
            println(s"curRow index: $curRowIndex")
            for (i <- 0 until n) {
              val outBigInt = dut.io.curRow.bits.value(i).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exResult(curRowIndex.toInt)(i)
              checkResult(out, expected, rowIdx, i, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curRow.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.io.done.expect(true.B)
        resCnt += 1
      }
    }.join()
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
  // "MultiFMA_v2 " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new MultiFMA_v2(k = 4, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testMultiFMA_v2[Float](dut)
  //     }
  // }
  // "MultiFMA_v2 " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new MultiFMA_v2(k = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testMultiFMA_v2[Int](dut)
  //     }
  // }
}
