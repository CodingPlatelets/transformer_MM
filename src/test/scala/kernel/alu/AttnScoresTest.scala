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

class AttnScoresTest extends AnyFlatSpec with ChiselScalatestTester {

  private def testQKGen[T: Numeric: ClassTag](
    dut: QKGen
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType
    val caseNum = 10

    val testCases = Array.tabulate(caseNum) { i =>
      val inputToken = matInit[T](m, k)
      val weightQ = matInit[T](k, n)
      val weightK = matInit[T](k, n)
      val exQuery = mmul(inputToken, weightQ)
      val exKey = mmul(inputToken, weightK)
      (inputToken, weightQ, weightK, exQuery, exKey)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inputToken, weightQ, weightK, _, _) = testCases(cnt)
        if (
          dut.io.inputToken.ready.peekBoolean() &&
          dut.io.weightQ.ready.peekBoolean() &&
          dut.io.weightK.ready.peekBoolean()
        ) {
          println("test case " + cnt + ": inputToken, weightQ and weightK are ready")
          dut.io.inputToken.valid.poke(true.B)
          dut.io.weightQ.valid.poke(true.B)
          dut.io.weightK.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until k
          } {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.inputToken.valid.poke(false.B)
          dut.io.weightQ.valid.poke(false.B)
          dut.io.weightK.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, _, exQuery, exKey) = testCases(resCnt)
        val precision = 0.001f
        var invalidcnt = 0
        if (dut.io.Key.valid.peekBoolean() && dut.io.Query.valid.peekBoolean()) {
          dut.io.Key.ready.poke(true.B)
          dut.io.Query.ready.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            val outQBigInt = dut.io.Query.bits(row)(col).peekInt()
            val outKBigInt = dut.io.Key.bits(row)(col).peekInt()
            val outQ = fromBinaryBigInt[T](outQBigInt)
            val outK = fromBinaryBigInt[T](outKBigInt)
            val expectedQ = exQuery(row)(col)
            val expectedK = exKey(row)(col)
            checkResult(outQ, expectedQ, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
            checkResult(outK, expectedK, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println("case " + resCnt + ": Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.Key.ready.poke(false.B)
          dut.io.Query.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()
  }
  private def testQKGenWithReg[T: Numeric: ClassTag](
    dut: QKGenWithReg
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType
    val caseNum = 10

    val testCases = Array.tabulate(caseNum) { i =>
      val inputToken = matInit[T](m, k)
      val weightQ = matInit[T](k, n)
      val weightK = matInit[T](k, n)
      val exQuery = mmul(inputToken, weightQ)
      val exKey = mmul(inputToken, weightK)
      (inputToken, weightQ, weightK, exQuery, exKey)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inputToken, weightQ, weightK, _, _) = testCases(cnt)
        if (
          dut.io.inputToken.ready.peekBoolean() &&
          dut.io.weightQ.ready.peekBoolean() &&
          dut.io.weightK.ready.peekBoolean()
        ) {
          println("test case " + cnt + ": inputToken, weightQ and weightK are ready")
          dut.io.inputToken.valid.poke(true.B)
          dut.io.weightQ.valid.poke(true.B)
          dut.io.weightK.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until k
          } {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.inputToken.valid.poke(false.B)
          dut.io.weightQ.valid.poke(false.B)
          dut.io.weightK.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, _, exQuery, exKey) = testCases(resCnt)
        if (dut.io.Key.valid.peekBoolean() && dut.io.Query.valid.peekBoolean()) {
          dut.io.Key.ready.poke(true.B)
          dut.io.Query.ready.poke(true.B)
          val precision = 0.001f
          var invalidcnt = 0
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            val outQBigInt = dut.io.Query.bits(row)(col).peekInt()
            val outKBigInt = dut.io.Key.bits(row)(col).peekInt()
            val outQ = fromBinaryBigInt[T](outQBigInt)
            val outK = fromBinaryBigInt[T](outKBigInt)
            val expectedQ = exQuery(row)(col)
            val expectedK = exKey(row)(col)
            checkResult(outQ, expectedQ, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
            checkResult(outK, expectedK, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println("case " + resCnt + ": Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.Key.ready.poke(false.B)
          dut.io.Query.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()
  }

  private def testQKMul[T: Numeric: ClassTag](
    dut: QKMul
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    val caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inQuery = matInit[T](m, n)
      val inKey = matInit[T](m, n)
      val exScores = mmul(inQuery, inKey.transpose)
      (inQuery, inKey, exScores)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inQuery, inKey, _) = testCases(cnt)
        if (dut.io.Query.ready.peekBoolean() && dut.io.Key.ready.peekBoolean()) {
          println("test case " + cnt + ": Query and Key are ready")
          dut.io.Query.valid.poke(true.B)
          dut.io.Key.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            dut.io.Query.bits(row)(col).poke(toBinaryBigInt(inQuery(row)(col)).U)
            dut.io.Key.bits(row)(col).poke(toBinaryBigInt(inKey(row)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.Query.valid.poke(false.B)
          dut.io.Key.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exScores) = testCases(resCnt)
        if (dut.io.scores.valid.peekBoolean()) {
          dut.io.scores.ready.poke(true.B)
          val precision = 0.001f
          var invalidcnt = 0
          for {
            row <- 0 until m
            col <- 0 until m
          } {
            val outBigInt = dut.io.scores.bits(row)(col).peekInt()
            val out = fromBinaryBigInt[T](outBigInt)
            val expected = exScores(row)(col)
            checkResult(out, expected, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println("case " + resCnt + ": Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.scores.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()

  }

  private def testQKMulWithReg[T: Numeric: ClassTag](
    dut: QKMulWithReg
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    val caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inQuery = matInit[T](m, n)
      val inKey = matInit[T](m, n)
      val exScores = mmul(inQuery, inKey.transpose)
      (inQuery, inKey, exScores)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inQuery, inKey, _) = testCases(cnt)
        if (dut.io.Query.ready.peekBoolean() && dut.io.Key.ready.peekBoolean()) {
          println("test case " + cnt + ": Query and Key are ready")
          dut.io.Query.valid.poke(true.B)
          dut.io.Key.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            dut.io.Query.bits(row)(col).poke(toBinaryBigInt(inQuery(row)(col)).U)
            dut.io.Key.bits(row)(col).poke(toBinaryBigInt(inKey(row)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.Query.valid.poke(false.B)
          dut.io.Key.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exScores) = testCases(resCnt)
        if (dut.io.scores.valid.peekBoolean()) {
          dut.io.scores.ready.poke(true.B)
          val precision = 0.001f
          var invalidcnt = 0
          for {
            row <- 0 until m
            col <- 0 until m
          } {
            val outBigInt = dut.io.scores.bits(row)(col).peekInt()
            val out = fromBinaryBigInt[T](outBigInt)
            val expected = exScores(row)(col)
            checkResult(out, expected, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println("case " + resCnt + ": Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.scores.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()

  }

  private def testQKMulSingle[T: Numeric: ClassTag](
    dut: QKMulSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    val caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inQuery = matInit[T](m, n)
      val inKey = matInit[T](m, n)
      val exScores = mmul(inQuery, inKey.transpose)
      (inQuery, inKey, exScores)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inQuery, inKey, _) = testCases(cnt)
        if (dut.io.Query.ready.peekBoolean() && dut.io.Key.ready.peekBoolean()) {
          println("test case " + cnt + ": Query and Key are ready")
          dut.io.Query.valid.poke(true.B)
          dut.io.Key.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
          } {
            dut.io.Query.bits(row)(col).poke(toBinaryBigInt(inQuery(row)(col)).U)
            dut.io.Key.bits(row)(col).poke(toBinaryBigInt(inKey(row)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.Query.valid.poke(false.B)
          dut.io.Key.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, exScores) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curRowScores.valid.peekBoolean()) {
            dut.io.curRowScores.ready.poke(true.B)
            dut.io.curRowScores.ready.poke(true.B)
            val curRowIndex = dut.io.curRowScores.bits.index.peekInt()
            println(s"curRow index: $curRowIndex")
            dut.io.curRowScores.bits.index.expect(rowIdx.U)
            for {
              col <- 0 until m
            } {
              val outBigInt = dut.io.curRowScores.bits.value(col).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exScores(rowIdx)(col)
              checkResult(out, expected, rowIdx, col, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curRowScores.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.io.done.expect(true.B)
        resCnt += 1
      }
    }.join()

  }

  private def testAttnScores[T: Numeric: ClassTag](
    dut: AttnScores
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    var caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inputToken = matInit[T](m, k)
      val weightQ = matInit[T](k, n)
      val weightK = matInit[T](k, n)
      val exQuery = mmul(inputToken, weightQ)
      val exKey = mmul(inputToken, weightK)
      val exScores = mmul(exQuery, exKey.transpose)
      (inputToken, weightQ, weightK, exScores)
    }

    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inputToken, weightQ, weightK, _) = testCases(cnt)
        if (
          dut.io.inputToken.ready.peekBoolean() &&
          dut.io.weightQ.ready.peekBoolean() &&
          dut.io.weightK.ready.peekBoolean()
        ) {
          println("test case " + cnt + ": inputToken, weightQ and weightK are ready")
          dut.io.inputToken.valid.poke(true.B)
          dut.io.weightQ.valid.poke(true.B)
          dut.io.weightK.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until k
          } {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.inputToken.valid.poke(false.B)
          dut.io.weightQ.valid.poke(false.B)
          dut.io.weightK.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, _, exScores) = testCases(resCnt)
        if (dut.io.scores.valid.peekBoolean()) {
          dut.io.scores.ready.poke(true.B)
          val precision = 0.001f
          var invalidcnt = 0
          for {
            row <- 0 until m
            col <- 0 until m
          } {
            val outBigInt = dut.io.scores.bits(row)(col).peekInt()
            val out = fromBinaryBigInt[T](outBigInt)
            val expected = exScores(row)(col)
            checkResult(out, expected, row, col, precision) match {
              case Some(_) => invalidcnt += 1
              case None    => // right
            }
          }
          if (invalidcnt == 0) println(s"case $resCnt : Verification passed!")
          else println(s"case $resCnt : Verification failed with $invalidcnt errors.")
          resCnt += 1
        } else {
          dut.io.scores.ready.poke(false.B)
        }
        dut.clock.step()
      }
    }.join()
  }

  private def testAttnScoresSingle[T: Numeric: ClassTag](
    dut: AttnScoresSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    var caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inputToken = matInit[T](m, k)
      val weightQ = matInit[T](k, n)
      val weightK = matInit[T](k, n)
      val exQuery = mmul(inputToken, weightQ)
      val exKey = mmul(inputToken, weightK)
      val exScores = mmul(exQuery, exKey.transpose)
      (inputToken, weightQ, weightK, exScores)
    }
    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inputToken, weightQ, weightK, _) = testCases(cnt)
        if (
          dut.io.inputToken.ready.peekBoolean() &&
          dut.io.weightQ.ready.peekBoolean() &&
          dut.io.weightK.ready.peekBoolean()
        ) {
          println("test case " + cnt + ": inputToken, weightQ and weightK are ready")
          dut.io.inputToken.valid.poke(true.B)
          dut.io.weightQ.valid.poke(true.B)
          dut.io.weightK.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until k
          } {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.inputToken.valid.poke(false.B)
          dut.io.weightQ.valid.poke(false.B)
          dut.io.weightK.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, _, exScores) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curRowScores.valid.peekBoolean()) {
            dut.io.curRowScores.ready.poke(true.B)
            val curRowIndex = dut.io.curRowScores.bits.index.peekInt()
            println(s"curRow index: $curRowIndex")
            dut.io.curRowScores.bits.index.expect(rowIdx.U)
            for {
              col <- 0 until m
            } {
              val outBigInt = dut.io.curRowScores.bits.value(col).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exScores(rowIdx)(col)
              checkResult(out, expected, rowIdx, col, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curRowScores.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.io.done.expect(true.B)
        resCnt += 1
      }
    }.join()
  }

  private def testAttnScoresSingleQueue[T: Numeric: ClassTag](
    dut: AttnScoresSingleQueue
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    var caseNum = 10
    val testCases = Array.tabulate(caseNum) { i =>
      val inputToken = matInit[T](m, k)
      val weightQ = matInit[T](k, n)
      val weightK = matInit[T](k, n)
      val exQuery = mmul(inputToken, weightQ)
      val exKey = mmul(inputToken, weightK)
      val exScores = mmul(exQuery, exKey.transpose)
      (inputToken, weightQ, weightK, exScores)
    }
    fork {
      var cnt = 0
      while (cnt < caseNum) {
        val (inputToken, weightQ, weightK, _) = testCases(cnt)
        dut.io.flush.poke(true.B)
        dut.clock.step()
        dut.io.flush.poke(false.B)
        if (
          dut.io.inputToken.ready.peekBoolean() &&
          dut.io.weightQ.ready.peekBoolean() &&
          dut.io.weightK.ready.peekBoolean()
        ) {
          println("test case " + cnt + ": inputToken, weightQ and weightK are ready")
          dut.io.inputToken.valid.poke(true.B)
          dut.io.weightQ.valid.poke(true.B)
          dut.io.weightK.valid.poke(true.B)
          for {
            row <- 0 until m
            col <- 0 until n
            i <- 0 until k
          } {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
          cnt += 1
        } else {
          dut.io.inputToken.valid.poke(false.B)
          dut.io.weightQ.valid.poke(false.B)
          dut.io.weightK.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resCnt = 0
      while (resCnt < caseNum) {
        val (_, _, _, exScores) = testCases(resCnt)
        var rowIdx = 0
        while (rowIdx < m) {
          val precision = 0.001f
          var invalidcnt = 0
          if (dut.io.curRowScores.valid.peekBoolean()) {
            dut.io.curRowScores.ready.poke(true.B)
            val curRowIndex = dut.io.curRowScores.bits.index.peekInt()
            println(s"curRow index: $curRowIndex")
            dut.io.curRowScores.bits.index.expect(rowIdx.U)
            for {
              col <- 0 until m
            } {
              val outBigInt = dut.io.curRowScores.bits.value(col).peekInt()
              val out = fromBinaryBigInt[T](outBigInt)
              val expected = exScores(rowIdx)(col)
              checkResult(out, expected, rowIdx, col, precision) match {
                case Some(_) => invalidcnt += 1
                case None    => // right
              }
            }
            if (invalidcnt == 0) println(s"case $resCnt : row $rowIdx Verification passed!")
            else println(s"case $resCnt : row $rowIdx Verification failed with $invalidcnt errors.")
            rowIdx += 1
          } else {
            dut.io.curRowScores.ready.poke(false.B)
          }
          dut.clock.step()
        }
        dut.io.done.expect(true.B)
        resCnt += 1
      }
    }.join()
  }

  "AttnScoresSingleQueue " should "compute fxp matrix multiplication" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new AttnScoresSingleQueue(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        testAttnScoresSingleQueue[Int](dut)
      }
  }

  // "AttnScoresSingleQueue " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new AttnScoresSingleQueue(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScoresSingleQueue[Float](dut)
  //     }
  // }

  // "AttnScoresSingle " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new AttnScoresSingle(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScoresSingle[Int](dut)
  //     }
  // }

  // "AttnScoresSingle " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new AttnScoresSingle(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScoresSingle[Float](dut)
  //     }
  // }

  // "QKMulSingle " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMulSingle(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMulSingle[Int](dut)
  //     }
  // }

  // "AttnScores " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new AttnScores(m = 4, k = 4, n = 4, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScores[Int](dut)
  //     }
  // }

  // "AttnScores " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new AttnScores(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScores[Float](dut)
  //     }
  // }

  // "QKMul " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMul(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMul[Int](dut)
  //     }
  // }

  // "QKMulWithReg " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMulWithReg(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMulWithReg[Int](dut)
  //     }
  // }

  // "QKGen " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKGen(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKGen[Int](dut)
  //     }
  // }

  // "QKGenWithReg " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKGenWithReg(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKGenWithReg[Int](dut)
  //     }
  // }
}
