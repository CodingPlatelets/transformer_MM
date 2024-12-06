package kernel.alu

import chisel3._
import chisel3.util._
import hardfloat._

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def ieeeQNaN = ((BigInt(1) << (ieeeWidth - 1)) - (BigInt(1) << (sig - 2))).U(ieeeWidth.W)
  def qNaN = ((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2))).U(recodedWidth.W)
  def isNaN(x:  UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === 3.U

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < 2.U
    val isSubnormal = code === 1.U || codeHi === 1.U && isHighSubnormalIn
    val isNormal = codeHi === 1.U && !isHighSubnormalIn || codeHi === 2.U
    val isZero = code === 0.U
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig - 2)
    val isQNaN = isNaN && x(sig - 2)

    Cat(
      isQNaN,
      isSNaN,
      isInf && !sign,
      isNormal && !sign,
      isSubnormal && !sign,
      isZero && !sign,
      isZero && sign,
      isSubnormal && sign,
      isNormal && sign,
      isInf && sign
    )
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x
  else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1 << to.exp).U) - (1 << exp).U
      Mux(expCode === 0.U || expCode >= 6.U, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  private def ieeeBundle = {
    val expWidth = exp
    class IEEEBundle extends Bundle {
      val sign = Bool()
      val exp = UInt(expWidth.W)
      val sig = UInt((ieeeWidth - expWidth - 1).W)
    }
    new IEEEBundle
  }

  def unpackIEEE(x: UInt) = x.asTypeOf(ieeeBundle)

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x:   UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

case class FPUParams(
  minFLen:     Int = 32,
  fLen:        Int = 64,
  divSqrt:     Boolean = true,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4,
  fpmuLatency: Int = 2,
  ifpuLatency: Int = 2)

object FPConstants {
  val RM_SZ = 3
  val FLAGS_SZ = 5
}

class FPInput(implicit p: FPUParams) extends Bundle {
  val rm = Bits(FPConstants.RM_SZ.W)
  val fmaCmd = Bits(2.W)
  val typ = Bits(2.W)
  val fmt = Bits(2.W)
  val in1 = Bits((p.fLen + 1).W)
  val in2 = Bits((p.fLen + 1).W)
  val in3 = Bits((p.fLen + 1).W)
}

class FPResult(implicit p: FPUParams) {
  val data = Bits((p.fLen + 1).W)
  val exc = Bits(FPConstants.FLAGS_SZ.W)
}

class MulAddRecFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module {
  override def desiredName = s"MulAddRecFNPipe_l${latency}_e${expWidth}_s${sigWidth}"
  require(latency <= 2)

  val io = IO(new Bundle {
    val validin = Input(Bool())
    val op = Input(Bits(2.W))
    val a = Input(Bits((expWidth + sigWidth + 1).W))
    val b = Input(Bits((expWidth + sigWidth + 1).W))
    val c = Input(Bits((expWidth + sigWidth + 1).W))
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))
    val out = Output(Bits((expWidth + sigWidth + 1).W))
    val exceptionFlags = Output(Bits(5.W))
    val validout = Output(Bool())
  })

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------

  val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
  val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

  mulAddRecFNToRaw_preMul.io.op := io.op
  mulAddRecFNToRaw_preMul.io.a := io.a
  mulAddRecFNToRaw_preMul.io.b := io.b
  mulAddRecFNToRaw_preMul.io.c := io.c

  val mulAddResult =
    (mulAddRecFNToRaw_preMul.io.mulAddA *
      mulAddRecFNToRaw_preMul.io.mulAddB) +&
      mulAddRecFNToRaw_preMul.io.mulAddC

  val valid_stage0 = Wire(Bool())
  val roundingMode_stage0 = Wire(UInt(3.W))
  val detectTininess_stage0 = Wire(UInt(1.W))

  val postmul_regs = if (latency > 0) 1 else 0
  mulAddRecFNToRaw_postMul.io.fromPreMul := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
  mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
  mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
  roundingMode_stage0 := Pipe(io.validin, io.roundingMode, postmul_regs).bits
  detectTininess_stage0 := Pipe(io.validin, io.detectTininess, postmul_regs).bits
  valid_stage0 := Pipe(io.validin, false.B, postmul_regs).valid

  //------------------------------------------------------------------------
  //------------------------------------------------------------------------

  val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

  val round_regs = if (latency == 2) 1 else 0
  roundRawFNToRecFN.io.invalidExc := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
  roundRawFNToRecFN.io.in := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
  roundRawFNToRecFN.io.roundingMode := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
  roundRawFNToRecFN.io.detectTininess := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
  io.validout := Pipe(valid_stage0, false.B, round_regs).valid

  roundRawFNToRecFN.io.infiniteExc := false.B

  io.out := roundRawFNToRecFN.io.out
  io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}
