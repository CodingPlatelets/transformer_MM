package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag

class CustomGemmTest extends AnyFlatSpec with ChiselScalatestTester with ParallelTestExecution {

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

  val precision = 0.001f
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
  def toSignedBigInt(value: BigInt, width: Int): BigInt = {
    val signBit = (value >> (width - 1)) & 1

    if (signBit == 1) {
      val maxValue = BigInt(1) << width
      value - maxValue
    } else {
      value
    }
  }

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

  private def testCustomGemm[T: Numeric: ClassTag]
  (dut: CustomGemm)
  (implicit config: DataWidthConfig) = {
    dut.clock.setTimeout(0)
    val p = dut.p
    val q = dut.q
    val m = dut.m
    val arraySize = 1
    val matrixAArray = Array.tabulate(arraySize)(_ => matInit[T](p, m))
    val matrixBArray = Array.tabulate(arraySize)(_ => matInit[T](m, q))
    val matrixArray = matrixAArray.zip(matrixBArray).map {
      case (a, b) => mmul(a, b)
    }
    //printmat((matrixAArray(0).flatten), p, m)
    //printmat((matrixBArray(0).flatten), m, q)
    printmat((matrixArray(0).flatten), p, q)
    def checkresult(): List[T] = {
      val ret = for (i <- 0 until p * q) yield {
        val out = fromBinaryBigInt[T](dut.io.out.bits(i).peekInt())
        // printf(cf"${out} ")
        // if((i+1)%q==0){
        //   println()
        // }
        out
      }
      ret.toList
    }   

    fork {
      var c = 0;
      while (c < arraySize) {
        if (dut.io.in_a.ready.peekBoolean() && dut.io.in_b.ready.peekBoolean()) {
          dut.io.in_a.valid.poke(true.B)
          dut.io.in_b.valid.poke(true.B)
          for (i <- 0 until m) {
            for (j <- 0 until p) {
              dut.io.in_a.bits(j * m + i).poke(toBinaryBigInt(matrixAArray(c)(j)(i)).U)
            }
            for (j <- 0 until q) {
              dut.io.in_b.bits(i * q + j).poke(toBinaryBigInt(matrixBArray(c)(i)(j)).U)
            }
          }
          c += 1
        } else {
          dut.io.in_a.valid.poke(false.B)
          dut.io.in_b.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resC = 0
      while (resC < arraySize) {
        if (dut.io.out.valid.peekBoolean()) {
          dut.io.out.ready.poke(true.B)
          val out = checkresult()
          printmat(out.toArray, p, q)
          var invalidcnt = 0
          var cnt = 0
          for (i <- out.zip(matrixArray(resC).flatten.toList)) {
            val isInvalid = (implicitly[ClassTag[T]].runtimeClass match {
              case c if c == classOf[Float] =>
                math.abs(i._1.asInstanceOf[Float] - i._2.asInstanceOf[Float]) > precision
              case c if c == classOf[Double] =>
                math.abs(i._1.asInstanceOf[Double] - i._2.asInstanceOf[Double]) > precision
              case c if c == classOf[Int] =>
                math.abs(i._1.asInstanceOf[Int] - i._2.asInstanceOf[Int]) > precision
              case _ =>
                throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")
            })
            if (isInvalid) {
              println(f"Error: ${cnt} ${i._1} ${i._2}")
              invalidcnt += 1
            }
            cnt = cnt + 1
          } 
          if (invalidcnt == 0) println("GEMM Verification passed!")
          assert(invalidcnt == 0)
          resC += 1
        } else {
          dut.io.out.ready.poke(false.B)
        }
        dut.clock.step()
      }

    }.join()
  }
 
  "CustomGemm basic test on Verilator" should "pass" in {
    implicit val fxpConfig: DataWidthConfig = FxpConfig
    test(new CustomGemm(4, 4, 4, 4, GEMMDataType.Fxp))
    .withAnnotations(Seq(VerilatorBackendAnnotation))(testCustomGemm[Float])
  }
}
