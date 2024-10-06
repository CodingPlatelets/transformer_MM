package pe.utils

import chisel3._
import chisel3.util._
import fixedpoint._
import vitiskernel.util.DebugLog

class FxpZoom(val WII: Int = 8, val WIF: Int = 8, val WOI: Int = 8, val WOF: Int = 8, val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val in = Input(UInt((WII + WIF).W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val inr = Reg(UInt((WII + WOF).W))
  val ini = inr(WII + WOF - 1, WOF)
  val outf = Reg(UInt(WOF.W))
  val outi = Reg(UInt(WOI.W))
  val overflow = RegInit(false.B)

  val tempInr = io.in(WII + WIF - 1, (WIF - WOF).abs)

  // BP flow
  if (WOF > WIF) {
    inr := Cat(io.in, Fill(WOF - WIF, 0.U))
  } else if (WOF == WIF || !ROUND) {
    inr := tempInr
  } else {
    if (WII + WOF >= 2) {
      when(io.in(WIF - WOF - 1) && ~(~tempInr(WII + WOF - 1) && (tempInr(WII + WOF - 2, 0).andR))) {
        inr := tempInr +& 1.U
      }.otherwise {
        inr := tempInr
      }
    } else {
      when(io.in(WIF - WOF - 1) && tempInr(WII + WOF - 1)) {
        inr := tempInr +& 1.U
      }.otherwise {
        inr := tempInr
      }
    }
  }

  // INT flow
  if (WOI >= WII) {
    overflow := false.B
    outi := ini.asSInt.pad(WOI).asUInt
    outf := inr(WOI - 1, 0)
  } else {
    when(!ini(WII - 1) && ini(WII - 2, WOI - 1).orR) {
      overflow := true.B
      outi := 0.U ## Fill(WOI - 1, 1.U)
      outf := Fill(WOF, 1.U)
    }.elsewhen(ini(WII - 1) && !(ini(WII - 2, WOI - 1).andR)) {
      overflow := true.B
      outi := 1.U ## Fill(WOI - 1, 0.U)
      outf := Fill(WOF, 0.U)
    }.otherwise {
      overflow := false.B
      outi := ini(WOI - 1, 0)
      outf := inr(WOF - 1, 0)
    }
  }

  // combine

  io.out := Cat(outi, outf)
  io.overflow := overflow

  debugLog(
    p"in: ${io.in}, inr: ${inr}, ini: ${ini}, outf: ${outf}, outi: ${outi}, overflow: ${overflow}, tempInr: ${tempInr}\n"
  )
}

class Fxp2Float(val WII: Int = 8, val WIF: Int = 8) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt((WII + WIF).W))
    val out = Output(UInt(32.W))
  })

  val ONEI = 1.U((WII + WIF).W)

  val sign = io.in(WII + WIF - 1)
  val inu = Mux(sign, (~io.in) + ONEI, io.in)

  val tail = RegInit(0.U(23.W))
  val flag = RegInit(false.B)
  val expz = RegInit(0.S(10.W))
  val ii = RegInit(22.S(10.W))

  tail := 0.U
  flag := false.B
  ii := 22.S
  expz := 0.S

  for (jj <- (WII + WIF - 1) to 0 by -1) {
    when(flag && ii >= 0.S) {
      tail := tail.bitSet(ii.asUInt, inu(jj))
      ii := ii - 1.S
    }
    when(inu(jj)) {
      when(!flag) {
        expz := (jj + 127 - WIF).S
      }
      flag := true.B
    }
  }

  val expt = Wire(UInt(8.W))
  when(expz < 255.S) {
    expt := Mux(inu === 0.U, 0.U, expz(7, 0).asUInt)
  }.otherwise {
    expt := 254.U
    tail := "h7FFFFF".U
  }

  io.out := Cat(sign, expt, tail)
}

class Fxp2FloatPipe(val WII: Int = 8, val WIF: Int = 8) extends Module with DebugLog {
  val io = IO(new Bundle {
    val rstn = Input(Bool())
    val clk = Input(Clock())
    val in = Input(UInt((WII + WIF).W))
    val out = Output(UInt(32.W))
  })

  val ONEI = 1.U((WII + WIF).W)

  val sign = Reg(Vec(WII + WIF + 1, Bool()))
  val exp = Reg(Vec(WII + WIF + 1, SInt(10.W)))
  val inu = Reg(Vec(WII + WIF + 1, UInt((WII + WIF).W)))

  val vall = RegInit(0.U(24.W))
  val valo = RegInit(0.U(24.W))
  val expo = RegInit(0.U(8.W))
  val signo = RegInit(false.B)

  io.out := Cat(signo, expo, valo(22, 0))

  for (ii <- 0 until WII + WIF + 1) {
    sign(ii) := false.B
    exp(ii) := 0.S
    inu(ii) := 0.U
  }

  when(!io.rstn) {
    for (ii <- 0 until WII + WIF + 1) {
      sign(ii) := false.B
      exp(ii) := 0.S
      inu(ii) := 0.U
    }
  }.otherwise {
    sign(WII + WIF) := io.in(WII + WIF - 1)
    exp(WII + WIF) := (WII + 127 - 1).S
    inu(WII + WIF) := Mux(io.in(WII + WIF - 1), (~io.in) + ONEI, io.in)
    for (ii <- (WII + WIF - 1) to 0 by -1) {
      sign(ii) := sign(ii + 1)
      when(inu(ii + 1)(WII + WIF - 1)) {
        exp(ii) := exp(ii + 1)
        inu(ii) := inu(ii + 1)
      }.otherwise {
        when(exp(ii + 1) =/= 0.S) {
          exp(ii) := exp(ii + 1) - 1.S
        }.otherwise {
          exp(ii) := exp(ii + 1)
        }
        inu(ii) := inu(ii + 1) << 1
      }
    }
  }

  if (23 > WII + WIF - 1) {
    vall := 0.U
    vall := inu(0)
  } else {
    vall := inu(0)(WII + WIF - 1, WII + WIF - 1 - 23)
  }

  when(!io.rstn) {
    signo := false.B
    expo := 0.U
    valo := 0.U
  }.otherwise {
    signo := sign(0)
    when(exp(0) >= 255.S) {
      expo := 255.U
      valo := "hFFFFFF".U
    }.elsewhen(exp(0) === 0.S || !vall(23)) {
      expo := 0.U
      valo := 0.U
    }.otherwise {
      expo := exp(0)(7, 0).asUInt
      valo := vall
    }
  }
}

class Float2Fxp(val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })
  val ONEO = 1.U((WOI + WOF).W)

  val sign = Reg(Bool())
  val exp2 = Wire(UInt(8.W))
  val valWire = Reg(UInt(24.W))
  val expi = Reg(SInt(32.W))
  val round = Reg(Bool())

  val overflow = WireDefault(false.B)
  val out = RegInit(0.U((WOI + WOF).W))
  debugLog(
    p"in: ${io.in}, sign: ${sign}, valWire: ${valWire}, exp2: ${exp2}, expi: ${expi}, overflow: ${overflow}, out: ${out}\n"
  )

  round := false.B
  sign := io.in(31)
  exp2 := io.in(30, 23)
  valWire := Cat(1.U(1.W), io.in(22, 0))

  expi := exp2.zext + (WOF - 127).S

  overflow := Mux(
    RegNext(exp2.andR)
      || expi >= (WOI + WOF - 1).S,
    true.B,
    false.B
  )
  when(RegNext(io.in(30, 0)) =/= 0.U && !RegNext(exp2.andR) && !overflow && expi >= -1.S) {
    round := (valWire(23, 25 - WOI - WOF) >> ((WOI + WOF).U - (expi + 1.S).asUInt)) & ROUND.B
    out := valWire(23, 24 - WOI - WOF) >> ((WOI + WOF).U - (expi + 1.S).asUInt) + round
    when(sign) {
      out := (~out).asUInt + ONEO
    }
  }

  when(overflow) {
    when(sign) {
      out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
    }.otherwise {
      out := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
    }
  }

  io.out := out
  io.overflow := RegNext(overflow)
}

class Float2FxpPipe(val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val ONEO = 1.U((WOI + WOF).W)

  // Input comb
  val sign = Wire(Bool())
  val exp = Wire(UInt(8.W))
  val valWire = Wire(UInt(24.W))

  sign := io.in(31)
  exp := io.in(30, 23)
  valWire := Cat(exp.orR, io.in(22, 0))

  // Pipeline stage 1
  val signinit = RegInit(false.B)
  val roundinit = RegInit(false.B)
  val expinit = RegInit(0.S(32.W))
  val outinit = RegInit(0.U((WOI + WOF).W))

  if (WOI + WOF - 1 >= 23) {
    outinit := 0.U(1.W) ## valWire ## 0.U
    roundinit := false.B
  } else {
    outinit := valWire(23, 23 - (WOI + WOF - 1))
    roundinit := (ROUND.B && valWire(23 - (WOI + WOF - 1) - 1))
  }

  signinit := sign
  when(exp === 255.U || Cat(0.U(24.W), exp) > (WOI + 126).U) {
    expinit := 0.S
  }.otherwise {
    expinit := (Cat(0.U(24.W), exp).asSInt - (WOI - 1).S - 127.S)
  }

  // Next pipeline stages
  val signs = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val rounds = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val exps = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.S(32.W))))
  val outs = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WOI + WOF).W))))

  for (ii <- 0 until WOI + WOF) {
    signs(ii) := signs(ii + 1)
    when(exps(ii + 1) =/= 0.S) {
      outs(ii) := outs(ii + 1) << 1
      rounds(ii) := false.B
      exps(ii) := exps(ii + 1) + 1.S
    }.otherwise {
      outs(ii) := outs(ii + 1)
      rounds(ii) := rounds(ii + 1)
      exps(ii) := exps(ii + 1)
    }
  }
  debugLog(p"signs: ${signs},\n rounds: ${rounds},\n exps: ${exps},\n outs: ${outs}.\n \n")
  signs(WOI + WOF) := signinit
  rounds(WOI + WOF) := roundinit
  exps(WOI + WOF) := expinit
  outs(WOI + WOF) := outinit

  // Last 2nd pipeline stage
  val signl = RegInit(false.B)
  val outl = RegInit(0.U((WOI + WOF).W))
  val outt = Reg(UInt((WOI + WOF).W))

  outt := outs(0)
  when(ROUND.B && rounds(0) && !(outt.andR)) {
    outt := outt + 1.U
  }
  when(signs(0)) {
    signl := outt =/= 0.U
    outt := (~outt).asUInt + ONEO
  }.otherwise {
    signl := false.B
  }
  outl := outt

  // Last 1st pipeline stage: overflow control
  io.out := outl
  io.overflow := false.B
  when(signl) {
    when(!outl(WOI + WOF - 1)) {
      io.out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
      io.overflow := true.B
    }
  }.otherwise {
    when(outl(WOI + WOF - 1)) {
      io.out := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
      io.overflow := true.B
    }
  }
}

class FxpAddSub(
  val WIIA: Int = 8,
  val WIFA: Int = 8,
  val WIIB: Int = 8,
  val WIFB: Int = 8,
  val WOI:  Int = 8,
  val WOF:  Int = 8)
    extends Module {
  val io = IO(new Bundle {
    val ina = Input(SInt((WIIA + WIFA).W))
    val inb = Input(SInt((WIIB + WIFB).W))
    // true for subtraction, false for addition
    val sub = Input(common.AddOrSub())
    val out = Output(SInt((WOI + WOF).W))
  })

  val middleWire = Wire(FixedPoint((WOI + WOF).W, WOF.BP))

  middleWire := io.ina.asFixedPoint(WIFA.BP) +& io.inb.asFixedPoint(WIFB.BP)

  io.out := RegNext(middleWire.asSInt)
}

class FxpMul(
  val WIIA: Int = 8,
  val WIFA: Int = 8,
  val WIIB: Int = 8,
  val WIFB: Int = 8,
  val WOI:  Int = 8,
  val WOF:  Int = 8)
    extends Module {
  val io = IO(new Bundle {
    val ina = Input(SInt((WIIA + WIFA).W))
    val inb = Input(SInt((WIIB + WIFB).W))
    val out = Output(SInt((WOI + WOF).W))
  })

  val middleWire = Wire(FixedPoint((WOI + WOF).W, WOF.BP))
  middleWire := io.ina.asFixedPoint(WIFA.BP) * io.inb.asFixedPoint(WIFB.BP)

  io.out := middleWire.asUInt

}

// pipeline stage = WOI + WOF + 3
// class FxpDivPipe(
//   val WIIA: Int = 8,
//   val WIFA: Int = 8,
//   val WIIB: Int = 8,
//   val WIFB: Int = 8,
//   val WOI:  Int = 8,
//   val WOF:  Int = 8)
//     extends Module {
//   val io = IO(new Bundle {
//     val dividend = Input(Valid(UInt((WIIA + WIFA).W)))
//     val divisor = Input(Valid(UInt((WIIB + WIFB).W)))
//     val out = Valid(UInt((WOI + WOF).W))
//     val overflow = Valid(Bool())
//   })

//   val WRI = if (WOI + WIIB > WIIA) WOI + WIIB else WIIA
//   val WRF = if (WOF + WIFB > WIFA) WOF + WIFB else WIFA

//   val divd = Wire(UInt((WRI + WRF).W))
//   val divr = Wire(UInt((WRI + WRF).W))
//   val roundedres = RegInit(0.U((WOI + WOF).W))
//   val rsign = RegInit(false.B)
//   val sign = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
//   val acc = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
//   val divdp = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
//   val divrp = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
//   val validPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
//   val res = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WOI + WOF).W))))

//   val ONEO = 1.U((WOI + WOF).W)
//   val ONEA = 1.U((WIIA + WIFA).W)
//   val ONEB = 1.U((WIIB + WIFB).W)

//   // Convert dividend and divisor to positive numbers
//   val udividend = Mux(io.dividend.bits(WIIA + WIFA - 1), (~io.dividend.bits) + ONEA, io.dividend)
//   val udivisor = Mux(io.divisor.bits(WIIB + WIFB - 1), (~io.divisor.bits) + ONEB, io.divisor)

//   val dividendZoom = Module(new FxpZoom(WIIA, WIFA, WRI, WRF, false))
//   val divisorZoom = Module(new FxpZoom(WIIB, WIFB, WRI, WRF, false))
//   dividendZoom.io.in := udividend
//   divd := dividendZoom.io.out

//   divisorZoom.io.in := udivisor
//   divr := divisorZoom.io.out

//   val tmp = RegInit(0.U((WRI + WRF).W))
//   // 1st pipeline stage: convert dividend and divisor to positive numbers
//   acc(0) := 0.U
//   divdp(0) := divd
//   divrp(0) := divr
//   validPipe(0) := io.dividend.valid && io.divisor.valid
//   io.out.valid := ShiftRegister(validPipe(WOI + WOF), 2)
//   sign(0) := io.dividend.bits(WIIA + WIFA - 1) ^ io.divisor.bits(WIIB + WIFB - 1)

//   // From 2nd to WOI + WOF + 1 pipeline stages: calculate division
//   for (ii <- 0 until WOI + WOF) {
//     res(ii + 1) := res(ii)
//     divdp(ii + 1) := divdp(ii)
//     divrp(ii + 1) := divrp(ii)
//     sign(ii + 1) := sign(ii)
//     validPipe(ii + 1) := validPipe(ii)
//     if (ii < WOI) tmp := acc(ii) + (divrp(ii) << (WOI - 1 - ii))
//     else tmp := acc(ii) + (divrp(ii) >> (1 + ii - WOI))
//     when(tmp < divdp(ii)) {
//       acc(ii + 1) := tmp
//       res(ii + 1)(WOF + WOI - 1 - ii) := true.B
//     }.otherwise {
//       acc(ii + 1) := acc(ii)
//       res(ii + 1)(WOF + WOI - 1 - ii) := false.B
//     }
//   }

//   // Next pipeline stage: process round
//   when(
//     ROUND.B && !res(WOI + WOF).andR && (acc(WOI + WOF) + (divrp(WOI + WOF) >> WOF) - divdp(WOI + WOF)) < (divdp(
//       WOI + WOF
//     ) - acc(WOI + WOF))
//   ) {
//     roundedres := res(WOI + WOF) + ONEO
//   }.otherwise {
//     roundedres := res(WOI + WOF)
//   }
//   rsign := sign(WOI + WOF)

//   // The last pipeline stage: process roof and output
//   io.overflow := false.B
//   when(rsign) {
//     when(roundedres(WOI + WOF - 1)) {
//       io.overflow := Mux(roundedres(WOI + WOF - 2, 0).orR, true.B, false.B)
//       io.out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
//     }.otherwise {
//       io.out.bits := (~roundedres) + ONEO
//     }
//   }.otherwise {
//     when(roundedres(WOI + WOF - 1)) {
//       io.overflow := true.B
//       io.out.bits := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
//     }.otherwise {
//       io.out.bits := roundedres
//     }
//   }
// }
