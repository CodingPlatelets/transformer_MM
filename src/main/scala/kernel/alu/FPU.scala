package kernel.alu

import chisel3._
import chisel3.util._
import hardfloat._
object FPU {
  def equivRecFN(expWidth: Int, sigWidth: Int, a: UInt, b: UInt) = {
    val top4A = a(expWidth + sigWidth, expWidth + sigWidth - 3)
    val top4B = b(expWidth + sigWidth, expWidth + sigWidth - 3)
    Mux(
      (top4A(2, 0) === 0.U) || (top4A(2, 0) === 7.U),
      (top4A === top4B) && (a(sigWidth - 2, 0) === b(sigWidth - 2, 0)),
      Mux((top4A(2, 0) === 6.U), (top4A === top4B), (a === b))
    )
  }
}

class ValExec_MulRecFN(expWidth: Int, sigWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(Bits((expWidth + sigWidth).W))
    val b = Input(Bits((expWidth + sigWidth).W))
    val roundingMode = Input(UInt(3.W))
    val detectTininess = Input(UInt(1.W))

    val expected = new Bundle {
      val out = Input(Bits((expWidth + sigWidth).W))
      val exceptionFlags = Input(Bits(5.W))
      val recOut = Output(Bits((expWidth + sigWidth + 1).W))
    }

    val actual = new Bundle {
      val out = Output(Bits((expWidth + sigWidth + 1).W))
      val exceptionFlags = Output(Bits(5.W))
    }

    val check = Output(Bool())
    val pass = Output(Bool())
  })

  val mulRecFN = Module(new MulRecFN(expWidth, sigWidth))
  mulRecFN.io.a := recFNFromFN(expWidth, sigWidth, io.a)
  mulRecFN.io.b := recFNFromFN(expWidth, sigWidth, io.b)
  mulRecFN.io.roundingMode := io.roundingMode
  mulRecFN.io.detectTininess := io.detectTininess

  io.expected.recOut := recFNFromFN(expWidth, sigWidth, io.expected.out)

  io.actual.out := mulRecFN.io.out
  io.actual.exceptionFlags := mulRecFN.io.exceptionFlags

  io.check := true.B
  io.pass :=
    FPU.equivRecFN(expWidth, sigWidth, io.actual.out, io.expected.recOut) &&
      (io.actual.exceptionFlags === io.expected.exceptionFlags)
}
