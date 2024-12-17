package models.llama3

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag
import kernel.alu.{DataWidthConfig, Fp32Config, Fp64Config, FxpConfig, GEMMDataType}

class metrixControllerTest extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

  // 辅助函数：矩阵乘法计算
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

  private def testMatrixSplit[T: Numeric: ClassTag](dut: MatrixSplit)(implicit config: DataWidthConfig): Unit = {
    val m = dut.m
    val p = dut.p
    val nk = dut.nk

    val inputMatrix = matInit[T](m, p)

    for {
      i <- 0 until m
      j <- 0 until p
    } {
      dut.io.inMatrix(i * p + j).poke(toBinaryBigInt(inputMatrix(i)(j)).U)
    }

    for {
      blockRow <- 0 until m / nk
      blockCol <- 0 until p / nk
    } {
      val blockIdx = blockRow * (p / nk) + blockCol
      val expectedValue = {
        var result = ""
        for {
          i <- 0 until nk
          j <- 0 until nk
        } {
          val value = inputMatrix(blockRow * nk + i)(blockCol * nk + j)

          // because the outBlock is a reverse order, so we need to concat the value in reverse order
          result = toBinaryString(value).concat(result)
        }
        result
      }

      dut.io.outBlocks(blockIdx).expect(BigInt(expectedValue, 2).U)
    }
  }

  // MatrixRestoreWarper
  class MatrixRestoreWarper(val m: Int, val p: Int, val nk: Int)(implicit config: DataWidthConfig) extends Module {
    val io = IO(new Bundle {
      val inMatrix = Input(Vec(m * p, UInt((config.inputWidth).W)))
      val outMatrix = Output(Vec(m * p, UInt(config.inputWidth.W)))
    })

    val matrixSplit = Module(new MatrixSplit(m, p, nk))
    val matrixRestore = Module(new MatrixRestore(m, p, nk))

    matrixSplit.io.inMatrix := io.inMatrix
    matrixRestore.io.inBlocks := matrixSplit.io.outBlocks
    io.outMatrix := matrixRestore.io.outMatrix
  }

  private def testMatrixRestore[T: Numeric: ClassTag](
    dut: MatrixRestoreWarper
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val p = dut.p
    val nk = dut.nk

    val inputMatrix = matInit[T](m, p)
    for {
      i <- 0 until m
      j <- 0 until p
    } {
      dut.io.inMatrix(i * p + j).poke(toBinaryBigInt(inputMatrix(i)(j)).U)
    }

    for {
      i <- 0 until m
      j <- 0 until p
    } {
      dut.io.outMatrix(i * p + j).expect(toBinaryBigInt(inputMatrix(i)(j)).U)
    }
  }

  class MetrixControllerWarper(
    val k:        Int,
    val n:        Int,
    val m:        Int,
    val p:        Int,
    val q:        Int,
    val gemmType: GEMMDataType.Type
  )(
    implicit config: DataWidthConfig)
      extends Module {
    val nk = n * k
    val io = IO(new Bundle {
      val in_a = Flipped(Decoupled(Vec(m * p, UInt(config.inputWidth.W))))
      val in_b = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
      val outMatrix = Valid(Vec(nk * nk, UInt(config.inputWidth.W)))
      val rowIdx = Output(UInt(config.inputWidth.W))
      val colIdx = Output(UInt(config.inputWidth.W))
    })

    val metrixController = Module(new GenerationMatrixMul(k, n, m, p, q, gemmType))
    metrixController.io.in_a <> io.in_a
    metrixController.io.in_b <> io.in_b
    metrixController.io.reset := false.B
    val matrixRestore = Module(new BlockMatrixRestore(nk))
    matrixRestore.io.inBlocks := metrixController.io.current.bits.value
    io.outMatrix.bits := matrixRestore.io.outMatrix
    io.outMatrix.valid := metrixController.io.current.valid
    io.rowIdx := metrixController.io.current.bits.row
    io.colIdx := metrixController.io.current.bits.col
  }

  private def testMetrixController[T: Numeric: ClassTag](
    dut: MetrixControllerWarper
  )(
    implicit config: DataWidthConfig
  ): Unit = {
    val m = dut.m
    val p = dut.p
    val q = dut.q
    val gemmType = dut.gemmType

    val inputMatrixA = matInit[T](m, p)
    val inputMatrixB = matInit[T](p, q)

    for {
      i <- 0 until m
      j <- 0 until p
    } {
      dut.io.in_a.bits(i * p + j).poke(toBinaryBigInt(inputMatrixA(i)(j)).U)
    }

    for {
      i <- 0 until p
      j <- 0 until q
    } {
      dut.io.in_b.bits(i * q + j).poke(toBinaryBigInt(inputMatrixB(i)(j)).U)
    }

    val nk = dut.n * dut.k
    val finalMatrix = mmul(inputMatrixA, inputMatrixB)
    println(s"inputMatrixA")
    printmat(inputMatrixA)
    println(s"inputMatrixB")
    printmat(inputMatrixB)
    println("finalMatrix:")
    printmat(finalMatrix)
    dut.io.in_a.valid.poke(true.B)
    dut.io.in_b.valid.poke(true.B)
    dut.clock.step()
    dut.io.in_a.valid.poke(false.B)
    dut.io.in_b.valid.poke(false.B)

    val allTimes = m * q / nk / nk
    var cnt = 0
    while (cnt < allTimes) {
      if (dut.io.outMatrix.valid.peekBoolean()) {
        var emptyRes = Array.fill(nk * nk)(BigInt(0))
        cnt += 1
        for {
          i <- 0 until nk
          j <- 0 until nk
        } {
          // TODO: change type
          emptyRes(i * nk + j) = dut.io.outMatrix.bits(i * nk + j).peekInt()
        }
        // println(s"emptyRes: ${emptyRes.mkString(", ")}")
        // assert(emptyRes.sameElements(finalMatrix))
        println(s"rowIdx: ${dut.io.rowIdx.peekInt()}")
        println(s"colIdx: ${dut.io.colIdx.peekInt()}")
        printmat(emptyRes, nk, nk)
      }
      dut.clock.step()
    }

  }

  // 测试用例
  "MatrixSplit" should "correctly split matrix into blocks" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new MatrixSplit(m = 4, p = 4, nk = 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation))(testMatrixSplit[Int])
  }

  "MatrixRestore" should "correctly restore matrix from blocks" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new MatrixRestoreWarper(m = 4, p = 4, nk = 2))
      .withAnnotations(Seq(VerilatorBackendAnnotation))(testMatrixRestore[Int])
  }

  "GenerationMatrixMul" should "correctly multiply matrices" in {
    implicit val config: DataWidthConfig = FxpConfig
    test(new MetrixControllerWarper(k = 1, n = 2, m = 4, p = 6, q = 8, GEMMDataType.Fxp))
      .withAnnotations(Seq(VerilatorBackendAnnotation))(testMetrixController[Int])
  }
}
