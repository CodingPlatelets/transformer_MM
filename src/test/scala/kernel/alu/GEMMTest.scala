package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

class GEMMTest extends AnyFlatSpec with ChiselScalatestTester {
  def mmul(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    for (r <- a) yield {
      for (c <- b.transpose) yield r.zip(c).map(Function.tupled(_ * _)).reduceLeft(_ + _)
    }
  }

  // n * n
  def matInit(n: Int): Array[Array[Int]] = {
    val rseed = System.currentTimeMillis().toInt
    // val rseed = 200
    val maxval = 5
    val rnd = new scala.util.Random(rseed)

    Array.tabulate(n) { _ => Array.tabulate(n) { _ => rnd.nextInt(maxval + 1) + 1 } }
  }

  def printmat(m: Array[Array[Int]]): Unit = {
    m.foreach { r => r.foreach { v => print(f"$v%4d") }; print(" ;") }
    println()
  }

  private def testGEMM(dut: GEMM) = {
    val n = dut.n
    val a = matInit(n)
    val b = matInit(n)
    val y = mmul(a, b)
    printmat(a)
    printmat(b)
    printmat(y)

    for (i <- 0 until n) {
      for (j <- 0 until n) {
        dut.InputA(i)(j).poke(a(i)(j))
        dut.InputB(i)(j).poke(b(i)(j))
      }
    }
    dut.DataReady.poke(true.B)

    dut.clock.step(3 * n)

    dut.OutputPipe.valid.expect(true.B)
    def checkresult(): List[Int] = {
      val ret = for (j <- 0 until n * n) yield {
        val out = dut.OutputPipe.bits(j).peekInt()
        print(out.toString + " ")
        out.toInt // litValue returns BigInt
      }
      println()
      ret.toList
    }

    val out = checkresult()
    var invalidcnt = 0

    for (i <- out.zip(y.flatten.toList)) {
      if (i._1 != i._2) {
        println("Error: " + i._1 + " " + i._2)
        invalidcnt += 1
      }
    }
    if (invalidcnt == 0) println("GEMM Verification passed!")
    assert(invalidcnt == 0)
  }

  private def testSystolicMM(dut: SystolicMM): Unit = {
    val n = dut.n
    val a = matInit(n)
    val b = matInit(n)
    val y = mmul(a, b)

    printmat(a)
    printmat(b)
    printmat(y)

    def checkresult(): List[Int] = {
      val ret = for (j <- 0 until n * n) yield {
        val out = dut.io.out(j).peek().litValue
        print(out.toString + " ")
        out.toInt // litValue returns BigInt
      }
      println()
      ret.toList
    }

    for (clk <- 0 until 2 * n) {
      for (idx <- 0 until n) {
        val p = clk - idx
        if (p >= 0 && p < n) {
          dut.io.in_a(idx).poke(a(idx)(p))
          dut.io.in_b(idx).poke(b(p)(idx))
        } else {
          dut.io.in_a(idx).poke(0)
          dut.io.in_b(idx).poke(0)
        }
      }
      dut.clock.step()
      print(f"$clk: ")
      checkresult()
    }
    dut.clock.step(n - 2) //  double check n-2 is correct

    val output = checkresult()

    var invalidcnt = 0
    for (i <- output.zip(y.flatten.toList)) {
      if (i._1 != i._2) {
        println("Error: " + i._1 + " " + i._2)
        invalidcnt += 1
      }
    }

    if (invalidcnt == 0) println("Verification passed!")
    assert(invalidcnt == 0)
    dut.clock.step(3)
  }

  private def testProcElem(dut: ProcElem): Unit = {
    require(dut.bits > 4)

    val hins = List(1, 2, 3, 0)
    val vins = List(4, 5, 6, 0)
    var acc = 0
    println("ProcElemUnitTester")
    for (i <- vins.zip(hins)) {
      dut.io.out.expect(acc)
      val h = i._1
      val v = i._2
      acc += h * v
      dut.io.in_h.poke(h)
      dut.io.in_v.poke(v)
      val out_h = dut.io.out_h.peek()
      val out_v = dut.io.out_v.peek()
      val out = dut.io.out.peek()
      println(s"h=$h v=$v out_h=$out_h out_v=$out_v out=$out")
      dut.clock.step()
    }
    dut.io.out.expect(acc)
  }

  "ProcElem basic test on Verilator" should "pass" in {
    test(new ProcElem()).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testProcElem)
  }

  "SystolicMM basic test on Verilator" should "pass" in {
    test(new SystolicMM(5, 16)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testSystolicMM)
  }

  "GeMM basic test on Verilator" should "pass" in {
    test(new GEMM(6, 16)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testGEMM)
  }
}

class GEMMFxpTest extends AnyFlatSpec with ChiselScalatestTester {
  def mmul(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    for (r <- a) yield {
      for (c <- b.transpose) yield r.zip(c).map(Function.tupled(_ * _)).reduceLeft(_ + _)
    }
  }

  // n * n
  def matInit(n: Int): Array[Array[Int]] = {
    val rseed = System.currentTimeMillis().toInt
    // val rseed = 200
    val maxval = 5
    val rnd = new scala.util.Random(rseed)

    Array.tabulate(n) { _ => Array.tabulate(n) { _ => rnd.nextInt(maxval + 1) + 1 } }
  }

  def printmat(m: Array[Array[Int]]): Unit = {
    m.foreach { r => r.foreach { v => print(f"$v%4d") }; print(" ;") }
    println()
  }

  def doubleToFixedPoint(d: Double, intBits: Int, fracBits: Int): BigInt = {
    // 检查数值范围
    val maxVal = Math.pow(2, intBits - 1) - Math.pow(2, -fracBits)
    val minVal = -Math.pow(2, intBits - 1)
    require(d <= maxVal && d >= minVal, s"Value $d out of range [$minVal, $maxVal]")

    // 转换为定点数表示
    BigInt((d * (1 << fracBits)).round)
  }

  private def testPEFxp(dut: PEFxp): Unit = {

    val hins = List(1.1, 2.2, 3.3, 0)
    val vins = List(4.4, 5.5, 6.6, 0)
    var acc = 0.0
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
      val out = dut.io.out.peekInt().toDouble / (1 << (2 * dut.F))
      println(
        f"h=${h}%.4f\t v=${v}%.4f\t out_h=${out_h}%.4f\t out_v=${out_v}%.4f\t out=${out}%.8f\t realAcc=$acc%.4f\n"
      )
      dut.clock.step()
    }
    // dut.io.out.expect(acc)
  }

  "PEFxp basic test on Verilator" should "pass" in {
    test(new PEFxp()).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(testPEFxp)
  }
}
