package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

class GEMMTest extends AnyFlatSpec with ChiselScalatestTester {

  def mmul(a: Array[Array[Float]], b: Array[Array[Float]]): Array[Array[Float]] = {
    for (r <- a) yield {
      for (c <- b.transpose) yield r.zip(c).map(Function.tupled(_ * _)).reduceLeft(_ + _)
    }
  }

  val precision = 0.001f
  // n * n
  def matInit(n: Int): Array[Array[Float]] = {
    val rseed = System.currentTimeMillis().toInt
    // val rseed = 200
    val maxval = 5.0f
    val rnd = new scala.util.Random(rseed)

    Array.tabulate(n) { _ => Array.tabulate(n) { _ => maxval * rnd.nextFloat() + 1 } }
  }

  def printmat(m: Array[Array[Float]]): Unit = {
    m.foreach { r => r.foreach { v => print(f"${v}%.2f ") }; print(" ;") }
    println()
  }

  def doubleToFixedPoint(d: Float, intBits: Int, fracBits: Int): BigInt = {
    // 检查数值范围
    val maxVal = Math.pow(2, intBits - 1) - Math.pow(2, -fracBits)
    val minVal = -Math.pow(2, intBits - 1)
    require(d <= maxVal && d >= minVal, s"Value $d out of range [$minVal, $maxVal]")

    // 转换为定点数表示
    BigInt((d * (1L << fracBits)).round)
  }

  private def testGEMM(dut: GEMM) = {
    val n = dut.n

    val arraySize = 10
    val matrixAArray = Array.tabulate(arraySize)(_ => matInit(n))
    val matrixBArray = Array.tabulate(arraySize)(_ => matInit(n))
    val matrixYArray = matrixAArray.zip(matrixBArray).map {
      case (a, b) => mmul(a, b)
    }

    def checkresult(): List[Float] = {
      val ret = for (j <- 0 until n * n) yield {
        val out = BigDecimal(dut.OutputPipe.bits(j).peekInt()) / (math.pow(2, 2 * dut.F))
        print(f"${out}%.4f ")
        out.toFloat // litValue returns BigInt
      }
      println()
      ret.toList
    }

    fork {
      var c = 0;
      while (c < arraySize) {
        if (dut.InputA.ready.peekBoolean() && dut.InputB.ready.peekBoolean()) {
          dut.InputA.valid.poke(true.B)
          dut.InputB.valid.poke(true.B)
          for (i <- 0 until n) {
            for (j <- 0 until n) {
              dut.InputA.bits(i)(j).poke(doubleToFixedPoint(matrixAArray(c)(i)(j), dut.I, dut.F))
              dut.InputB.bits(i)(j).poke(doubleToFixedPoint(matrixBArray(c)(i)(j), dut.I, dut.F))
            }
          }
          c += 1
        } else {
          dut.InputA.valid.poke(false.B)
          dut.InputB.valid.poke(false.B)
        }
        dut.clock.step()
      }
    }.fork {
      var resC = 0
      while (resC < arraySize) {
        if (dut.OutputPipe.valid.peekBoolean()) {
          dut.OutputPipe.ready.poke(true.B)
          val out = checkresult()
          var invalidcnt = 0
          for (i <- out.zip(matrixYArray(resC).flatten.toList)) {
            if (math.abs(i._1 - i._2) > precision) {
              println("Error: " + i._1 + " " + i._2)
              invalidcnt += 1
            }
          }
          if (invalidcnt == 0) println("GEMM Verification passed!")
          assert(invalidcnt == 0)
          resC += 1
        } else {
          dut.OutputPipe.ready.poke(false.B)
        }
        dut.clock.step()
      }

    }.join()

  }

  private def testSystolicMM(dut: SystolicMM): Unit = {
    val n = dut.n
    val a = matInit(n)
    val b = matInit(n)
    val y = mmul(a, b)

    printmat(a)
    printmat(b)
    printmat(y)

    def checkresult(): List[Float] = {
      val ret = for (j <- 0 until n * n) yield {
        val out = BigDecimal(dut.io.out(j).peekInt()) / (math.pow(2, 2 * dut.F))
        print(f"${out}%.4f ")
        out.toFloat // litValue returns BigInt
      }
      println()
      ret.toList
    }

    for (clk <- 0 until 2 * n) {
      for (idx <- 0 until n) {
        val p = clk - idx
        if (p >= 0 && p < n) {
          dut.io.in_a(idx).poke(doubleToFixedPoint(a(idx)(p), dut.I, dut.F))
          dut.io.in_b(idx).poke(doubleToFixedPoint(b(p)(idx), dut.I, dut.F))
        } else {
          dut.io.in_a(idx).poke(0)
          dut.io.in_b(idx).poke(0)
        }
      }
      dut.clock.step()
      print(f"clk: $clk: ")
      checkresult()
    }
    dut.clock.step(n - 2) //  double check n-2 is correct

    val output = checkresult()

    var invalidcnt = 0
    for (i <- output.zip(y.flatten.toList)) {
      if (math.abs(i._1 - i._2) > precision) {
        println("Error: " + i._1 + " " + i._2)
        invalidcnt += 1
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    assert(invalidcnt == 0)
    dut.clock.step(3)
  }

  private def testPEFxp(dut: PEFxp): Unit = {
    val hins = List(1.1f, 2.2f, 3.3f, 0)
    val vins = List(4.4f, 5.5f, 6.6f, 0)
    var acc = 0.0f
    println("ProcElemUnitTester")
    for (i <- vins.zip(hins)) {
      // dut.io.out.expect(acc)
      val h = i._1
      val v = i._2
      // acc += h * v
      acc += h * v
      dut.io.in_h.poke(doubleToFixedPoint(h, dut.I, dut.F))
      dut.io.in_v.poke(doubleToFixedPoint(v, dut.I, dut.F))
      val out_h = dut.io.out_h.peekInt().toDouble / (1 << (dut.F))
      val out_v = dut.io.out_v.peekInt().toDouble / (1 << (dut.F))
      val out = BigDecimal(dut.io.out.peekInt()) / (1 * math.pow(2, 2 * dut.F))

      println(
        f"h=${h}%.4f\t v=${v}%.4f\t out_h=${out_h}%.4f\t out_v=${out_v}%.4f\t out=${out}%.4f\t realAcc=$acc%.4f\n"
      )
      dut.clock.step()
    }
    // dut.io.out.expect(acc)
  }

  private def testPEFp(dut: PEFp): Unit = {
    val hins = Seq(2.1f, 2.2f, 3.3f, 1123.1234f)
    val vins = Seq(4.4f, 5.5f, 6.6f, 3214.2314f)
    val result = hins.zip(vins).map(i => i._1 * i._2).sum
    println("PE float test")
    println(s"result: $result")

    dut.reset.poke(true.B)
    dut.io.reset.poke(true.B)
    dut.clock.step(1)
    dut.reset.poke(false.B)
    dut.io.reset.poke(false.B)
    dut.io.in_h.valid.poke(false.B)
    dut.io.in_v.valid.poke(false.B)

    fork {
      for ((i, j) <- vins.zip(hins)) {
        dut.io.in_h.valid.poke(true.B)
        dut.io.in_v.valid.poke(true.B)
        dut.io.in_h.bits.poke(BigInt(java.lang.Float.floatToRawIntBits(i).toBinaryString, 2).U)
        dut.io.in_v.bits.poke(BigInt(java.lang.Float.floatToRawIntBits(j).toBinaryString, 2).U)
        // dut.clock.step()
        // println(f"acc: $acc%.4f")
        dut.clock.step()
      }
      dut.io.in_h.valid.poke(false.B)
      dut.io.in_v.valid.poke(false.B)
      dut.io.in_h.bits.poke(0.U)
      dut.io.in_v.bits.poke(0.U)
      dut.clock.step(3)
    }.fork {
      dut.clock.step(11)
      val out = java.lang.Float.intBitsToFloat(dut.io.out.peekInt().toInt)
      assert(math.abs(out - result) < precision)
      // println(f"out: ${out}%.4f\t, result: $result%.4f")
      dut.clock.step(3)
    }.join()
  }

  // "PEFxp basic test on Verilator" should "pass" in {
  //   test(new PEFxp()).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testPEFxp)
  // }

  "PEFp basic test on Verilator" should "pass" in {
    test(new PEFp(32, 4)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testPEFp)
  }

  // "SystolicMM basic test on Verilator" should "pass" in {
  //   test(new SystolicMM(5)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testSystolicMM)
  // }

  // "GeMM basic test on Verilator" should "pass" in {
  //   test(new GEMM(6)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testGEMM)
  // }
}
