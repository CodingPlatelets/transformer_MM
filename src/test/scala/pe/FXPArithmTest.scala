package pe
import chisel3._
import chiseltest._
import pe.utils._
import org.scalatest.flatspec.AnyFlatSpec

class FXPArithmTest extends AnyFlatSpec with ChiselScalatestTester {

  val bit = 64
  val dimV = 32
  val depth = 128
  val annos = Seq(VerilatorBackendAnnotation)

  behavior.of("tester on fifo with memory")
  it should "fifo with it" in {
    test(new Float2FxpPipe)
      .withAnnotations(annos) { dut => }
  }

  behavior.of("tester on Float2Fxp zoom")
  it should "convert float to fixed point correctly" in {
    test(new Float2Fxp).withAnnotations(annos) { dut =>
      fork {
        // 测试正数转换
        dut.io.in.poke("h3f800000".U) // 1.0f
        dut.clock.step()

        // 测试负数转换
        dut.io.in.poke("hbf800000".U) // -1.0f
        dut.clock.step()

        // 测试溢出情况
        dut.io.in.poke("h7f7fffff".U) // 最大正数
        dut.clock.step()
        dut.io.in.poke("hff7fffff".U) // 最小负数
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("h0100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("hff00".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("h7fff".U)
        dut.io.overflow.expect(true.B)

        dut.clock.step()
        dut.io.out.expect("h8000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }

  behavior.of("tester on FxpZoom")
  it should "zoom fixedpoint correctly" in {
    test(new FxpZoom(8, 8, 7, 5, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      fork {
        // Test case 1: Carrier case
        dut.io.in.poke("b00000000_11111111".U) // 8-bit integer part and 8-bit fractional part
        dut.clock.step()

        // Test case 2: Round case
        dut.io.in.poke("b11111111_11111111".U) // Maximum value
        dut.clock.step()

        // Test case 3: normal case
        dut.io.in.poke("b00000000_10000001".U) // Value that should cause rounding
        dut.clock.step()

        // Test case 4: overflow
        dut.io.in.poke("b10000000_10000000".U) // Negative value
        dut.clock.step()
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("b0000001_00000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b000000_00000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b0000000_10000".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b1000000_00000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }

  behavior.of("FXP zoom extend")
  it should "zoom fixedpoint correctly" in {
    test(new FxpZoom(8, 8, 8, 9, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      fork {
        // Test case 1: Carrier case
        dut.io.in.poke("b00000000_11111111".U) // 8-bit integer part and 8-bit fractional part
        dut.clock.step()

        // Test case 2: Round case
        dut.io.in.poke("b11111111_11111111".U) // Maximum value
        dut.clock.step()

        // Test case 3: normal case
        dut.io.in.poke("b00000000_10000001".U) // Value that should cause rounding
        dut.clock.step()

        // Test case 4: overflow
        dut.io.in.poke("b10000000_10000000".U) // Negative value
        dut.clock.step()
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("b00000000_111111110".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b11111111_111111110".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b0000000_100000010".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b10000000_100000000".U)
        dut.io.overflow.expect(false.B)
      }.join()
    }
  }

  behavior.of("FxpAddSub")
  it should ("do add and sub correctly") in {
    test(new FxpAddSub(8, 8, 6, 5, 7, 7, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      fork {
        // Test case 1: Carrier case
        dut.io.ina.poke("b00000000_11111111".U)
        dut.io.inb.poke("b000000_11111".U)
        dut.io.sub.poke(common.AddOrSub.ADD)
        dut.clock.step()

        // Test case 2: Round case
        dut.io.ina.poke("b11111111_11111111".U) // Maximum value
        dut.io.inb.poke("b111111_11111".U) // Maximum value
        dut.io.sub.poke(common.AddOrSub.SUB)
        dut.clock.step()

        // Test case 3: overflow
        dut.io.ina.poke("b10000000_10000000".U) // Negative value
        dut.io.inb.poke("b100100_10000".U) // Negative value
        dut.io.sub.poke(common.AddOrSub.ADD)
        dut.clock.step()
      }.fork {
        dut.clock.step(2)
        dut.io.out.expect("b0000001_1111100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b0000000_0000100".U)
        dut.io.overflow.expect(false.B)

        dut.clock.step()
        dut.io.out.expect("b1000000_0000000".U)
        dut.io.overflow.expect(true.B)
      }.join()
    }
  }
}

class FXPMulDivTest extends AnyFlatSpec with ChiselScalatestTester {
  val WIIA = 12
  val WIFA = 20
  val WIIB = 14
  val WIFB = 18
  val WOI = 24
  val WOF = 17

  val testInput = Seq(
    ("a09b63b3", "1d320443"),
    ("8bb51e68", "761cf80d"),
    ("1d322443", "00000010"),
    ("6e56e35e", "4b45ead0"),
    ("9432d234", "1b86880c"),
    ("8bb55e68", "00000062"),
    ("2bb004db", "bd814b70"),
    ("39ad79bc", "6815ad29"),
    ("8bb5ce68", "00000042"),
    ("76de4b61", "c9809a37"),
    ("8bb5ce68", "00000000"),
    ("666f2bff", "43b2df79"),
    ("8bb5de68", "0000ffff"),
    ("00000000", "1d320443"),
    ("8bb5dece", "0000fffe"),
    ("00000000", "00000000"),
    ("7a164399", "1b35e411"),
    ("68d9b80a", "45cddeea"),
    ("b6ba294f", "4995af1b"),
    ("f6360551", "270bdea8"),
    ("a34728f2", "d4657725"),
    ("66b53c9c", "2211eeff"),
    ("b6b62e8e", "c70b04d5"),
    ("d70edf8b", "7181eff3"),
    ("6e546855", "f8ecca82"),
    ("680a9d44", "c699cee3"),
    ("f6c772c2", "34ccc642"),
    ("a2ad7ac4", "2b77d220"),
    ("a09b63b3", "1d320443"),
    ("8bb51e68", "761cf80d"),
    ("6e56e35e", "4b45ead0"),
    ("9432d234", "1b86880c"),
    ("2bb004db", "bd814b70"),
    ("39ad79bc", "6815ad29"),
    ("76de4b61", "c9809a37"),
    ("666f2bff", "43b2df79"),
    ("7a164399", "1b35e411"),
    ("68d9b80a", "45cddeea"),
    ("b6ba294f", "4995af1b"),
    ("f6360551", "270bdea8"),
    ("a34728f2", "d4657725"),
    ("66b53c9c", "2211eeff"),
    ("b6b62e8e", "c70b04d5"),
    ("d70edf8b", "7181eff3"),
    ("6e546855", "f8ecca82"),
    ("680a9d44", "c699cee3"),
    ("f6c772c2", "34ccc642"),
    ("a2ad7ac4", "2b77d220")
  )

  val zeroTuple = ("00000000", "00000000")

  def hex2Bin(hex: String): String = {
    BigInt(hex, 16).toString(2)
  }

  def hex2SignedInt(hex: String, width: Int) = {
    val maxUnsignedValue = BigInt(2).pow(width)
    val maxPositiveValue = BigInt(2).pow(width - 1)
    val resTmp = BigInt(hex, 16)
    if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
    else { resTmp }
  }

  val annos = Seq(VerilatorBackendAnnotation)
  behavior.of("FXPMulDiv")
  it should ("do mul and div") in {
    test(new FxpMul(WIIA, WIFA, WIIB, WIFB, WOI, WOF, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      fork {
        for ((a, b) <- testInput) {
          dut.io.ina.poke(("b" + hex2Bin(a)).U)
          dut.io.inb.poke(("b" + hex2Bin(b)).U)
          dut.clock.step()
        }
      }.fork {
        dut.clock.step(2)
        for ((a, b) <- testInput) {
          val maxUnsignedValue = BigInt(2).pow(WOF + WOI)
          val maxPositiveValue = BigInt(2).pow(WOF + WOI - 1)
          var aInt = hex2SignedInt(a, WIIA + WIFA).toDouble / (1 << WIFA)
          var bInt = hex2SignedInt(b, WIIB + WIFB).toDouble / (1 << WIFB)
          var abmul = aInt * bInt

          val resTmp = dut.io.out.peekInt()
          var res = (if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
                     else { resTmp }).toDouble / (1 << WOF)
          var over = dut.io.overflow.peekBoolean()

          println(f"a= $aInt%16f, b= $bInt%16f, a*b= $abmul%16f ($over%s)\t omul= $res%16f")

          dut.clock.step()
        }

        dut.io.ina.poke(BigInt(zeroTuple._1, 16).U)
        dut.io.ina.poke(BigInt(zeroTuple._2, 16).U)
        dut.clock.step(WOI + WOF + 8)
      }.join()

    }
  }

  it should ("do div") in {
    test(new FxpDiv(WIIA, WIFA, WIIB, WIFB, WOI, WOF, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      fork {
        for ((a, b) <- testInput) {
          dut.io.dividend.poke(("b" + hex2Bin(a)).U)
          dut.io.divisor.poke(("b" + hex2Bin(b)).U)
          dut.clock.step()
        }

        dut.io.dividend.poke(BigInt(zeroTuple._1, 16).U)
        dut.io.divisor.poke(BigInt(zeroTuple._2, 16).U)
      }.fork {
        dut.clock.step(WOI + WOF + 5)
        for ((a, b) <- testInput) {
          val maxUnsignedValue = BigInt(2).pow(WOF + WOI)
          val maxPositiveValue = BigInt(2).pow(WOF + WOI - 1)
          var aInt = hex2SignedInt(a, WIIA + WIFA).toDouble / (1 << WIFA)
          var bInt = hex2SignedInt(b, WIIB + WIFB).toDouble / (1 << WIFB)
          var abdiv = aInt / bInt

          val resTmp = dut.io.out.peekInt()
          var res = (if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
                     else { resTmp }).toDouble / (1 << WOF)
          var over = dut.io.overflow.peekBoolean()

          println(f"a= $aInt%16f, b= $bInt%16f, a/b= $abdiv%16f ($over%s)\t odiv= $res%16f")

          dut.clock.step()
        }

        dut.clock.step(WOI + WOF + 8)
      }.join()

    }
  }
}

class FXPSqrtTest extends AnyFlatSpec with ChiselScalatestTester {
  val WII = 10;
  val WIF = 10;
  val WOI = 6;
  val WOF = 12;
  val values = Seq(
    "f0d77",
    "96e31",
    "7f97f",
    "fffff",
    "caab9",
    "d957c",
    "1cd28",
    "2cd6b",
    "8506c",
    "e496f",
    "3dcd6",
    "00000",
    "80000",
    "e0fa9",
    "ea05f",
    "03f17",
    "856d1",
    "ce8c1",
    "a45dc",
    "0094f",
    "c9f55",
    "b70c2",
    "08061",
    "1e935",
    "e7eac",
    "9c397",
    "43c04",
    "5abbd",
    "5736b",
    "fbce6",
    "c4777",
    "e0da0",
    "b581a",
    "03fac",
    "60ffd",
    "07203",
    "9d2f9",
    "289aa",
    "07a47",
    "ecc5b",
    "c9965",
    "03643",
    "e5eed"
  )

  val zero = "00000"

  val annos = Seq(VerilatorBackendAnnotation)

  def hex2SignedInt(hex: String, width: Int) = {
    val maxUnsignedValue = BigInt(2).pow(width)
    val maxPositiveValue = BigInt(2).pow(width - 1)
    val resTmp = BigInt(hex, 16)
    if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
    else { resTmp }
  }
  val maxUnsignedValue = BigInt(2).pow(WOF + WOI)
  val maxPositiveValue = BigInt(2).pow(WOF + WOI - 1)

  behavior.of("FXPSqrt")
  it should ("do sqrt correctly") in {
    test(new FxpSqrt(WII, WIF, WOI, WOF, true)).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      fork {
        for (i <- values) {
          dut.io.in.poke(("b" + BigInt(i, 16).toString(2)).U)
          dut.clock.step()
        }

        // zero input
        dut.io.in.poke(("b" + BigInt(zero, 16).toString(2)).U)

      }.fork {
        dut.clock.step((WII + 2 - 1) / 2 + WIF + 2 + 1)

        for (i <- values) {

          val resTmp = dut.io.out.peekInt()
          val res = (if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
                     else { resTmp }).toDouble / (1 << WOF)
          val res2 = math.pow(res, 2)
          val input = hex2SignedInt(i, WII + WIF).toDouble / (1 << WIF)
          val over = if (dut.io.overflow.peekBoolean()) { "(O)" }
          else { "( )" }

          println(f"input = $input%16f,\t output = $res%12f $over, output^2 = $res2%12f")
          dut.clock.step()
        }

      }.join()
    }
  }
}
