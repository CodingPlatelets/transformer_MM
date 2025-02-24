package kernel.alu
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.ParallelTestExecution
import scala.reflect.ClassTag
import kernel.alu.{DataWidthConfig, Fp32Config, Fp64Config, FxpConfig, GEMMDataType}
import ujson.Arr

object Utils {
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
  def toBinaryString[T: Numeric: ClassTag](v: T)(implicit config: DataWidthConfig): String = {
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

  def checkResult[T: Numeric: ClassTag](
    out:       T,
    expected:  T,
    row:       Int,
    col:       Int,
    precision: Float
  ): Option[Unit] = {
    val isInvalid = implicitly[ClassTag[T]].runtimeClass match {
      case c if c == classOf[Float] =>
        math.abs(out.asInstanceOf[Float] - expected.asInstanceOf[Float]) > precision
      case c if c == classOf[Double] =>
        math.abs(out.asInstanceOf[Double] - expected.asInstanceOf[Double]) > precision
      case c if c == classOf[Int] =>
        math.abs(out.asInstanceOf[Int] - expected.asInstanceOf[Int]) > precision
      case _ =>
        throw new IllegalArgumentException(s"Unsupported type: ${implicitly[ClassTag[T]].runtimeClass}")
    }

    if (isInvalid) {
      println(s"Error: row: $row col: $col")
      printmat(Array(Array(out)))
      printmat(Array(Array(expected)))
      Some(())
    } else None
  }
}
