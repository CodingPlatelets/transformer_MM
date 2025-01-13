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

    val inputToken = matInit[T](m, k)
    val weightQ = matInit[T](k, n)
    val weightK = matInit[T](k, n)
    val Query = mmul(inputToken, weightQ)
    val Key = mmul(inputToken, weightK)

    printmat(Query)
    printmat(Key)

    if (
      dut.io.inputToken.ready.peekBoolean() &&
      dut.io.weightQ.ready.peekBoolean() &&
      dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
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
    } else {
      dut.io.inputToken.valid.poke(false.B)
      dut.io.weightQ.valid.poke(false.B)
      dut.io.weightK.valid.poke(false.B)
    }

    while (!(dut.io.Key.valid.peekBoolean() && dut.io.Query.valid.peekBoolean())) {
      dut.clock.step()
    }

    dut.io.Key.ready.poke(true.B)
    dut.io.Query.ready.poke(true.B)

    val precision = 0.001f
    var invalidcnt = 0
    for {
      row <- 0 until m
      col <- 0 until n
    } {
      val outBigInt = dut.io.Query.bits(row)(col).peekInt()
      val out = fromBinaryBigInt[T](outBigInt)
      val expected = Query(row)(col)
      checkResult(out, expected, row, col, precision) match {
        case Some(_) => invalidcnt += 1
        case None    => // right
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testQKMul[T: Numeric: ClassTag](
    dut: QKMul
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType

    val Query = matInit[T](m, n)
    val Key = matInit[T](m, n)
    val expectedResults = mmul(Query, Key.transpose)

    println("Query:")
    printmat(Query)
    println("Key:")
    printmat(Key)
    println("expectedResults:")
    printmat(expectedResults)

    if (dut.io.Query.ready.peekBoolean() && dut.io.Key.ready.peekBoolean()) {
      println("Query and Key are ready")
      dut.io.Query.valid.poke(true.B)
      dut.io.Key.valid.poke(true.B)

      for {
        row <- 0 until m
        col <- 0 until n
      } {
        dut.io.Query.bits(row)(col).poke(toBinaryBigInt(Query(row)(col)).U)
        dut.io.Key.bits(row)(col).poke(toBinaryBigInt(Key(row)(col)).U)
      }
    } else {
      dut.io.Query.valid.poke(false.B)
      dut.io.Key.valid.poke(false.B)
    }

    while (!dut.io.scores.valid.peekBoolean()) {
      dut.clock.step()
    }

    dut.io.scores.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0

    for {
      row <- 0 until m
      col <- 0 until m
    } {
      val outBigInt = dut.io.scores.bits(row)(col).peekInt()
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

  private def testQKMulSingle[T: Numeric: ClassTag](
    dut: QKMulSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    val Query = matInit[T](m, n)
    val Key = matInit[T](m, n)
    val expectedResults = mmul(Query, Key.transpose)

    printmat(Query)
    printmat(Key)
    printmat(expectedResults)

    if (dut.io.Query.ready.peekBoolean() && dut.io.Key.ready.peekBoolean()) {
      println("Query and Key are ready")
      dut.io.Query.valid.poke(true.B)
      dut.io.Key.valid.poke(true.B)
      for {
        row <- 0 until m
        col <- 0 until n
      } {
        dut.io.Query.bits(row)(col).poke(toBinaryBigInt(Query(row)(col)).U)
        dut.io.Key.bits(row)(col).poke(toBinaryBigInt(Key(row)(col)).U)
      }
    } else {
      dut.io.Query.valid.poke(false.B)
      dut.io.Key.valid.poke(false.B)
    }

    val precision = 0.001f
    var invalidcnt = 0

    while (!dut.io.done.peekBoolean()) {
      if (dut.io.curRowScores.valid.peekBoolean()) {
        val currentRowIndex = dut.io.curRowScores.bits.index.peekInt()
        println(s"currentRow index: $currentRowIndex")

        for (i <- 0 until m) {
          val outBigInt = dut.io.curRowScores.bits.value(i).peekInt()
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

  private def testAttnScores[T: Numeric: ClassTag](
    dut: AttnScores
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    val inputToken = matInit[T](m, k)
    val weightQ = matInit[T](k, n)
    val weightK = matInit[T](k, n)
    val Query = mmul(inputToken, weightQ)
    val Key = mmul(inputToken, weightK)
    val expectedResults = mmul(Query, Key.transpose)

    print("Query:\n")
    printmat(Query)
    print("Key:\n")
    printmat(Key)
    print("expectedResults:\n")
    printmat(expectedResults)

    if (
      dut.io.inputToken.ready.peekBoolean() &&
      dut.io.weightQ.ready.peekBoolean() &&
      dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
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
    } else {
      dut.io.inputToken.valid.poke(false.B)
      dut.io.weightQ.valid.poke(false.B)
      dut.io.weightK.valid.poke(false.B)
    }

    while (!dut.io.scores.valid.peekBoolean()) {
      dut.clock.step()
    }

    dut.io.scores.ready.poke(true.B)
    val precision = 0.001f
    var invalidcnt = 0

    for {
      row <- 0 until m
      col <- 0 until m
    } {
      val outBigInt = dut.io.scores.bits(row)(col).peekInt()
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

  private def testAttnScoresSingle[T: Numeric: ClassTag](
    dut: AttnScoresSingle
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    val inputToken = matInit[T](m, k)
    val weightQ = matInit[T](k, n)
    val weightK = matInit[T](k, n)
    val Query = mmul(inputToken, weightQ)
    val Key = mmul(inputToken, weightK)
    val expectedResults = mmul(Query, Key.transpose)

    print("Query:\n")
    printmat(Query)
    print("Key:\n")
    printmat(Key)
    print("expectedResults:\n")
    printmat(expectedResults)

    if (
      dut.io.inputToken.ready.peekBoolean() &&
      dut.io.weightQ.ready.peekBoolean() &&
      dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
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
    } else {
      dut.io.inputToken.valid.poke(false.B)
      dut.io.weightQ.valid.poke(false.B)
      dut.io.weightK.valid.poke(false.B)
    }

    val precision = 0.001f
    var invalidcnt = 0

    while (!dut.io.done.peekBoolean()) {
      if (dut.io.curRowScores.valid.peekBoolean()) {
        val currentRowIndex = dut.io.curRowScores.bits.index.peekInt()
        println(s"currentRow index: $currentRowIndex")

        for (i <- 0 until m) {
          val outBigInt = dut.io.curRowScores.bits.value(i).peekInt()
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

  private def testAttnScoresSingleQueue[T: Numeric: ClassTag](
    dut: AttnScoresSingleQueue
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val k = dut.k
    val n = dut.n
    val gemmType = dut.gemmType

    val inputToken = matInit[T](m, k)
    val weightQ = matInit[T](k, n)
    val weightK = matInit[T](k, n)
    val Query = mmul(inputToken, weightQ)
    val Key = mmul(inputToken, weightK)
    val expectedResults = mmul(Query, Key.transpose)

    print("Query:\n")
    printmat(Query)
    print("Key:\n")
    printmat(Key)
    print("expectedResults:\n")
    printmat(expectedResults)
    dut.io.flush.poke(true.B)
    dut.clock.step(1)
    dut.io.flush.poke(false.B)
    if (
      dut.io.inputToken.ready.peekBoolean() &&
      dut.io.weightQ.ready.peekBoolean() &&
      dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
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
    } else {
      dut.io.inputToken.valid.poke(false.B)
      dut.io.weightQ.valid.poke(false.B)
      dut.io.weightK.valid.poke(false.B)
    }

    val precision = 0.001f
    var invalidcnt = 0

    while (!dut.io.done.peekBoolean()) {
      if (dut.io.curRowScores.valid.peekBoolean()) {
        val currentRowIndex = dut.io.curRowScores.bits.index.peekInt()
        println(s"currentRow index: $currentRowIndex")

        for (i <- 0 until m) {
          val outBigInt = dut.io.curRowScores.bits.value(i).peekInt()
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

  // "AttnScoresSingleQueue " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new AttnScoresSingleQueue(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScoresSingleQueue[Int](dut)
  //     }
  // }

  "AttnScoresSingleQueue " should "compute fxp matrix multiplication" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new AttnScoresSingleQueue(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        testAttnScoresSingleQueue[Int](dut)
      }
  }

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
  // "QKMul " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMul(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMul[Int](dut)
  //     }
  // }
  // "QKMulSingle " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMulSingle(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMulSingle[Int](dut)
  //     }
  // }
//   "AttnScores " should "compute fxp matrix multiplication" in {
//     implicit val config: DataWidthConfig = FxpConfig
//     test(new AttnScores(m = 4, k = 4, n = 4, peCount = 4, gemmType = GEMMDataType.Fxp))
//       .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
//         testAttnScores[Int](dut)
//       }
//   }

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

//   "QKMulWithReg " should "compute fxp matrix multiplication" in {
//     implicit val config: DataWidthConfig = FxpConfig
//     test(new QKMulWithReg(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
//       .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
//         testQKMulWithReg[Int](dut)
//       }
//   }

  // "QKGen " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKGen(m = 8, k = 8, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKGen[Int](dut)
  //     }
  // }

//   "QKGenWithReg " should "compute fxp matrix multiplication" in {
//     implicit val config: DataWidthConfig = FxpConfig
//     test(new QKGenWithReg(m = 8, k = 8, n = 8,  peCount = 4, gemmType = GEMMDataType.Fxp))
//       .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
//         testQKGenWithReg[Int](dut)
//       }
//   }
}
