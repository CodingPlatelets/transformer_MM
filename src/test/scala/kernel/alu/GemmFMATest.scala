package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag
import kernel.alu.{DataWidthConfig, Fp32Config, Fp64Config, FxpConfig, GEMMDataType}

class GEMMFMATest extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

  def mmul[T: Numeric: ClassTag](a: Array[Array[T]], b: Array[Array[T]]): Array[Array[T]] = {
    val rows = a.length
    val cols = b(0).length
    val n = b.length
    val num = implicitly[Numeric[T]]

    Array.tabulate(rows, cols) { (i, j) =>
      var sum = num.zero
      for (k <- 0 until n) {
        sum = num.plus(sum, num.times(a(i)(k), b(k)(j)))
      }
      sum
    }
  }

  def matInit[T: Numeric: ClassTag](
    rows: Int,
    cols: Int
  )(
    implicit config: DataWidthConfig
  ): Array[Array[T]] = {
    val r = new scala.util.Random(42)
    val ct = implicitly[ClassTag[T]]
    val numeric = implicitly[Numeric[T]]

    ct.runtimeClass match {
      case c if c == classOf[Int] =>
        // 定点数使用 -8 到 7 的整数
        Array.fill(rows, cols)(
          numeric.fromInt(
            // r.nextInt(math.pow(2, config.inputWidth).toInt) - math.pow(2, config.inputWidth - 1).toInt
            r.nextInt(4) - 2
          )
        )
      case c if c == classOf[Float] =>
        // 32位浮点数使用 -1 到 1 的随机浮点数
        // Float 类型
        Array.fill(rows, cols)((r.nextFloat() * 2 - 1).asInstanceOf[T])
      case c if c == classOf[Double] =>
        // 64位浮点数使用 -1 到 1 的随机浮点数
        Array.fill(rows, cols)((r.nextDouble() * 2 - 1).asInstanceOf[T])
      case _ =>
        throw new IllegalArgumentException(s"不支持的数据类型: ${ct.runtimeClass}")
    }
  }

  def toSignedBigInt(value: BigInt, width: Int): BigInt = {
    val signBit = (value >> (width - 1)) & 1

    if (signBit == 1) {
      val maxValue = BigInt(1) << width
      value - maxValue
    } else {
      value
    }
  }

  def printmat[T: Numeric: ClassTag](m: Array[Array[T]]): Unit = {
    val numeric = implicitly[Numeric[T]]
    val ct = implicitly[ClassTag[T]]

    m.foreach { r =>
      r.foreach { v =>
        ct.runtimeClass match {
          case c if c == classOf[Float] =>
            print(f"${v.asInstanceOf[Float]}%.4f\t")
          case c if c == classOf[Double] =>
            print(f"${v.asInstanceOf[Double]}%.4f\t")
          case c if c == classOf[Int] =>
            print(f"${v.asInstanceOf[Int]}%d\t")
          case _ =>
            throw new IllegalArgumentException(s"不支持的数据类型: ${ct.runtimeClass}")
        }
      }
      println(";")
    }
    println()
  }

  def printmat[T: Numeric: ClassTag](m: Array[T], x: Int, y: Int)(implicit config: DataWidthConfig): Unit = {
    val numeric = implicitly[Numeric[T]]
    val ct = implicitly[ClassTag[T]]

    for (i <- 0 until x) {
      for (j <- 0 until y) {
        ct.runtimeClass match {
          case c if c == classOf[Float] =>
            print(f"${m(i * y + j).asInstanceOf[Float]}%.4f\t")
          case c if c == classOf[Double] =>
            print(f"${m(i * y + j).asInstanceOf[Double]}%.4f\t")
          case c if c == classOf[Int] =>
            print(f"${m(i * y + j).asInstanceOf[Int]}%d\t")
          case c if c == classOf[BigInt] =>
            print(f"${toSignedBigInt(m(i * y + j).asInstanceOf[BigInt], config.inputWidth)}%d\t")
          case _ =>
            throw new IllegalArgumentException(s"不支持的数据类型: ${ct.runtimeClass}")
        }
      }
      println(";")
    }
    println()
  }

  // convert T to binary bigInt
  def toBinaryBigInt[T: Numeric: ClassTag](v: T)(implicit config: DataWidthConfig): BigInt = {
    val ct = implicitly[ClassTag[T]]
    val num = implicitly[Numeric[T]]

    ct.runtimeClass match {
      case c if c == classOf[Int] =>
        val intValue = v.asInstanceOf[Int]
        // 使用 inputWidth 位来表示所有整数，保持符号位
        val mask = (1L << config.inputWidth) - 1
        BigInt(intValue) & mask
      case c if c == classOf[Float] =>
        BigInt(java.lang.Float.floatToRawIntBits(v.asInstanceOf[Float]).toBinaryString, 2)
      case c if c == classOf[Double] =>
        BigInt(java.lang.Double.doubleToRawLongBits(v.asInstanceOf[Double]).toBinaryString, 2)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported type: ${ct.runtimeClass}")
    }
  }

  // convrt T to binary string
  private def toBinaryString[T: Numeric: ClassTag](v: T)(implicit config: DataWidthConfig): String = {
    val ct = implicitly[ClassTag[T]]
    val num = implicitly[Numeric[T]]

    ct.runtimeClass match {
      case c if c == classOf[Int] =>
        val intBValue = v.asInstanceOf[Int].toBinaryString
        if (intBValue.length < config.inputWidth) {
          intBValue.reverse.padTo(config.inputWidth, '0').reverse
        } else {
          intBValue.takeRight(config.inputWidth)
        }
      case c if c == classOf[Float] =>
        java.lang.Float
          .floatToRawIntBits(v.asInstanceOf[Float])
          .toBinaryString
          .reverse
          .padTo(config.inputWidth, '0')
          .reverse
      case c if c == classOf[Double] =>
        java.lang.Double
          .doubleToRawLongBits(v.asInstanceOf[Double])
          .toBinaryString
          .reverse
          .padTo(config.inputWidth, '0')
          .reverse
      case _ =>
        throw new IllegalArgumentException(s"Unsupported type: ${ct.runtimeClass}")
    }
  }

  // convert binary bigInt to T
  def fromBinaryBigInt[T: Numeric: ClassTag](bigInt: BigInt)(implicit config: DataWidthConfig): T = {
    val ct = implicitly[ClassTag[T]]

    ct.runtimeClass match {
      case c if c == classOf[Int] =>
        val intValue = bigInt.toInt
        // 处理符号位
        val signExtendedValue = if ((intValue & (1 << (config.inputWidth - 1))) != 0) {
          intValue | ~((1 << config.inputWidth) - 1)
        } else {
          intValue
        }
        signExtendedValue.asInstanceOf[T]
      case c if c == classOf[Float] =>
        java.lang.Float.intBitsToFloat(bigInt.toInt).asInstanceOf[T]
      case c if c == classOf[Double] =>
        java.lang.Double.longBitsToDouble(bigInt.toLong).asInstanceOf[T]
      case _ =>
        throw new IllegalArgumentException(s"Unsupported type: ${ct.runtimeClass}")
    }
  }

  private def testMultiFMA[T: Numeric: ClassTag](
    dut: MultiFMA
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val k = dut.k
    val peCount = dut.peCount
    val gemmType = dut.gemmType

    // val fixedMatrix = Array(
    //   Array(4, 2, 3, 1)
    // )
    // val fixedMatrix2 = Array(
    //   Array(4, 2, 3, 1),
    //   Array(0, 5, 1, 3),
    //   Array(4, 2, 1, 0),
    //   Array(0, 3, 1, 3)
    // )
    // val matrixA_row = fixedMatrix
    // val matrixB_cols = fixedMatrix2
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
      for (i <- matrixA_row(0).indices) {
        for (j <- 0 until peCount) {
          dut.io.matrixA_row.bits(i).poke(toBinaryBigInt(matrixA_row(0)(i)).U)
          dut.io.matrixB_cols.bits(i)(j).poke(toBinaryBigInt(matrixB_cols(i)(j)).U)
        }
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
      val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
        case c if c == classOf[Float] =>
          math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
        case c if c == classOf[Double] =>
          math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision
        case c if c == classOf[Int] =>
          math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
        case _ =>
          throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")
      })

      if (isInvalid) {
        println("Error: ")
        printmat(Array(Array(out)))
        printmat(Array(Array(expected)))
        invalidcnt += 1
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
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
            dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
          }
        }
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

    for (row <- 0 until m) {
      for (col <- 0 until n) {
        val outBigInt = dut.io.results.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision
          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")
        })
        // printmat(Array(Array(out)))
        // printmat(Array(Array(expected)))
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
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
    // printmat(expectedResults)

    if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
      println("matrixA and matrixB are ready")
      dut.io.matrixA.valid.poke(true.B)
      dut.io.matrixB.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
            dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
          }
        }
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
        println("currentRow index: " + currentRowIndex)
        for (i <- 0 until n) {
          val outBigInt = dut.io.currentRow.bits.value(i).peekInt()
          val out = fromBinaryBigInt[T](outBigInt)
          val expected = expectedResults(currentRowIndex.toInt)(i)
          println("i: " + i)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
            case c if c == classOf[Float] =>
              math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
            case c if c == classOf[Double] =>
              math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision
            case c if c == classOf[Int] =>
              math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
            case _ =>
              throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")
          })
          if (isInvalid) {
            println("Error: ")
            printmat(Array(Array(out)))
            printmat(Array(Array(expected)))
            invalidcnt += 1
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
    printmat(expectedResults)

    if (dut.io.matrixA.ready.peekBoolean() && dut.io.matrixB.ready.peekBoolean()) {
      println("matrixA and matrixB are ready")
      dut.io.matrixA.valid.poke(true.B)
      dut.io.matrixB.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.matrixA.bits(row)(i).poke(toBinaryBigInt(matrixA(row)(i)).U)
            dut.io.matrixB.bits(i)(col).poke(toBinaryBigInt(matrixB(i)(col)).U)
          }
        }
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
        println("currentRow index: " + currentRowIndex)
        for (i <- 0 until n) {
          val outBigInt = dut.io.currentRow.bits.value(i).peekInt()
          val out = fromBinaryBigInt[T](outBigInt)
          val expected = expectedResults(currentRowIndex.toInt)(i)
          println("i: " + i)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
            case c if c == classOf[Float] =>
              math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
            case c if c == classOf[Double] =>
              math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

            case c if c == classOf[Int] =>
              math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
            case _ =>
              throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

          })
          if (isInvalid) {
            println("Error: ")
            printmat(Array(Array(out)))
            printmat(Array(Array(expected)))
            invalidcnt += 1
          }
        }
      }
      dut.clock.step()
    }
    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

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
    printmat(Query)
    val Key = mmul(inputToken, weightK)
    printmat(Key)

    if (
      dut.io.inputToken.ready.peekBoolean() && dut.io.weightQ.ready.peekBoolean() && dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
      dut.io.inputToken.valid.poke(true.B)
      dut.io.weightQ.valid.poke(true.B)
      dut.io.weightK.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until n) {
        val outBigInt = dut.io.Query.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = Query(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }

    for (row <- 0 until m) {
      for (col <- 0 until n) {
        val outBigInt = dut.io.Key.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = Key(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
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

    val inputToken = matInit[T](m, k)
    val weightQ = matInit[T](k, n)
    val weightK = matInit[T](k, n)
    val Query = mmul(inputToken, weightQ)
    printmat(Query)
    val Key = mmul(inputToken, weightK)
    printmat(Key)

    if (
      dut.io.inputToken.ready.peekBoolean() && dut.io.weightQ.ready.peekBoolean() && dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
      dut.io.inputToken.valid.poke(true.B)
      dut.io.weightQ.valid.poke(true.B)
      dut.io.weightK.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until n) {
        val outBigInt = dut.io.Query.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = Query(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }

    for (row <- 0 until m) {
      for (col <- 0 until n) {
        val outBigInt = dut.io.Key.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = Key(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testQKMulTotal[T: Numeric: ClassTag](
    dut: QKMulTotal
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType
    // println("m: " + m + " n: " + n)

    // val fixedMatrix = Array(
    //   Array(4, -1, 3, 1),
    //   Array(0, 5, -3, 3),
    //   Array(4, -2, 4, 0),
    //   Array(0, 3, -1, 3)
    // )
    // val Query = fixedMatrix
    // val Key = fixedMatrix
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
      println(" Query and Key are ready")
      dut.io.Query.valid.poke(true.B)
      dut.io.Key.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          dut.io.Query.bits(row)(col).poke(toBinaryBigInt(Query(row)(col)).U)
          dut.io.Key.bits(row)(col).poke(toBinaryBigInt(Key(row)(col)).U)
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until m) {
        val outBigInt = dut.io.scores.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }
    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  private def testQKMulTotalWithReg[T: Numeric: ClassTag](
    dut: QKMulTotalWithReg
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val n = dut.n
    val gemmType = dut.gemmType
    // println("m: " + m + " n: " + n)

    // val fixedMatrix = Array(
    //   Array(4, -1, 3, 1),
    //   Array(0, 5, -3, 3),
    //   Array(4, -2, 4, 0),
    //   Array(0, 3, -1, 3)
    // )
    // val Query = fixedMatrix
    // val Key = fixedMatrix
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
      println(" Query and Key are ready")
      dut.io.Query.valid.poke(true.B)
      dut.io.Key.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          dut.io.Query.bits(row)(col).poke(toBinaryBigInt(Query(row)(col)).U)
          dut.io.Key.bits(row)(col).poke(toBinaryBigInt(Key(row)(col)).U)
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until m) {
        val outBigInt = dut.io.scores.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
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
    printmat(Query)
    val Key = mmul(inputToken, weightK)
    printmat(Key.transpose)
    val expectedResults = mmul(Query, Key.transpose) // Query * Key^T
    printmat(expectedResults)

    if (
      dut.io.inputToken.ready.peekBoolean() && dut.io.weightQ.ready.peekBoolean() && dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
      dut.io.inputToken.valid.poke(true.B)
      dut.io.weightQ.valid.poke(true.B)
      dut.io.weightK.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until m) {
        val outBigInt = dut.io.scores.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }
    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }
  private def testAttnScoresTotal[T: Numeric: ClassTag](
    dut: AttnScoresTotal
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
    printmat(Query)
    val Key = mmul(inputToken, weightK)
    printmat(Key.transpose)
    val expectedResults = mmul(Query, Key.transpose) // Query * Key^T
    printmat(expectedResults)

    if (
      dut.io.inputToken.ready.peekBoolean() && dut.io.weightQ.ready.peekBoolean() && dut.io.weightK.ready.peekBoolean()
    ) {
      println("inputToken, weightQ and weightK are ready")
      dut.io.inputToken.valid.poke(true.B)
      dut.io.weightQ.valid.poke(true.B)
      dut.io.weightK.valid.poke(true.B)
      for (row <- 0 until m) {
        for (col <- 0 until n) {
          for (i <- 0 until k) {
            dut.io.inputToken.bits(row)(i).poke(toBinaryBigInt(inputToken(row)(i)).U)
            dut.io.weightQ.bits(i)(col).poke(toBinaryBigInt(weightQ(i)(col)).U)
            dut.io.weightK.bits(i)(col).poke(toBinaryBigInt(weightK(i)(col)).U)
          }
        }
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
    for (row <- 0 until m) {
      for (col <- 0 until m) {
        val outBigInt = dut.io.scores.bits(row)(col).peekInt()
        val out = fromBinaryBigInt[T](outBigInt)
        val expected = expectedResults(row)(col)
        val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
          case c if c == classOf[Float] =>
            math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
          case c if c == classOf[Double] =>
            math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision

          case c if c == classOf[Int] =>
            math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
          case _ =>
            throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")

        })
        if (isInvalid) {
          println("Error: row: " + row + " col: " + col)
          printmat(Array(Array(out)))
          printmat(Array(Array(expected)))
          invalidcnt += 1
        }
      }
    }
    if (invalidcnt == 0) println("Verification passed!")
    else println(s"Verification failed with $invalidcnt errors.")
  }

  // ===--::--===
  //  below tests ERROR
  // ===--::--===

  // "AttnScoresTotal " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new AttnScoresTotal(m = 4, k = 4, n = 4, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testAttnScoresTotal[Int](dut)
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

  // ===--::--===
  //  below tests PASS
  // ===--::--===

  // "QKMulTotal " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMulTotal(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMulTotal[Int](dut)
  //     }
  // }

  // "QKMulTotalWithReg " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKMulTotalWithReg(m = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKMulTotalWithReg[Int](dut)
  //     }
  // }

  // "QKGen " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKGen(m = 8, k = 8, n = 8,  peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKGen[Int](dut)
  //     }
  // }

  // "QKGenWithReg " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new QKGenWithReg(m = 8, k = 8, n = 8,  peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testQKGenWithReg[Int](dut)
  //     }
  // }

  // "GEMMSingleQueue " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new GEMMSingleQueue(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fxp))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMSingleQueue[Int](dut)
  //     }
  // }

  // "GEMMSingleQueue " should "compute fp32 matrix multiplication" in {
  //   implicit val config: DataWidthConfig = Fp32Config
  //   test(new GEMMSingleQueue(m = 8, k = 8, n = 12, peCount = 4, gemmType = GEMMDataType.Fp32))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       testGEMMSingleQueue[Float](dut)
  //     }
  // }

  // "GEMMFMATotal " should "compute fxp matrix multiplication" in {
  //   implicit val config: DataWidthConfig = FxpConfig
  //   test(new GEMMFMATotal(m = 4, k = 4, n = 8, peCount = 4, gemmType = GEMMDataType.Fxp))
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
