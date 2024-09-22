package pe.utils

import chisel3._
import chisel3.util._
import coursier.core.shaded.sourcecode.Macros.Chunk.Val

class FxpZoom(val WII: Int = 8, val WIF: Int = 8, val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1)
    extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt((WII + WIF).W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val inr = RegInit(0.U((WII + WOF).W))
  val ini = RegInit(0.U(WII.W))
  val outi = RegInit(0.U(WOI.W))
  val outf = RegInit(0.U(WOF.W))

  io.overflow := false.B

  if (WOF < WIF) {
    if (ROUND == 0) {
      inr := io.in(WII + WIF - 1, WIF - WOF)
    } else if (WII + WOF >= 2) {
      inr := Mux(
        io.in(WIF - WOF - 1) && !(~io.in(WII + WOF - 1) && io.in(WII + WOF - 2, 0).andR),
        io.in(WII + WIF - 1, WIF - WOF) + 1.U,
        io.in(WII + WIF - 1, WIF - WOF)
      )
    } else {
      inr := Mux(
        io.in(WIF - WOF - 1) && inr(WII + WOF - 1),
        io.in(WII + WIF - 1, WIF - WOF) + 1.U,
        io.in(WII + WIF - 1, WIF - WOF)
      )
    }
  } else if (WOF == WIF) {
    inr := io.in ## inr(WOF - WIF - 1, 0)
  } else {
    inr := io.in ## 0.U((WOF - WIF).W)
  }

  if (WOI < WII) {
    ini := inr(WII + WOF - 1, WOF)
    outf := inr(WOF - 1, 0)
    when(~ini(WII - 1) && ini(WII - 2, WOI - 1).orR) {
      io.overflow := true.B
      outi := 0.U(1.W) ## Fill(WOI - 1, 1.U)
      outf := Fill(WOF, 1.U)
    }.elsewhen(ini(WII - 1) && !(ini(WII - 2, WOI - 1).andR)) {
      io.overflow := true.B
      outi := 1.U(1.W) ## Fill(WOI - 1, 0.U)
      outf := 0.U
    }.otherwise {
      io.overflow := false.B
      outi := ini(WOI - 1, 0)
    }
  } else {
    ini := inr(WII + WOF - 1, WOF)
    outf := inr(WOF - 1, 0)
    io.overflow := false.B
    outi := Mux(ini(WII - 1), Fill(WOI - WII, 1.U(1.W)), Fill(WOI - WII, 0.U(1.W))) ## ini
  }

  io.out := Cat(outi, outf)
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

class Fxp2FloatPipe(val WII: Int = 8, val WIF: Int = 8) extends Module {
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

class Float2Fxp(val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val ONEO = 1.U((WOI + WOF).W)

  val sign = Reg(Bool())
  val exp2 = Reg(UInt(8.W))
  val valWire = Reg(UInt(24.W))
  val expi = Reg(SInt(32.W))
  val round = Reg(Bool())

  val overflow = Wire(Bool())
  val out = Wire(UInt((WOI + WOF).W))

  round := false.B
  overflow := false.B
  out := 0.U

  sign := io.in(31)
  exp2 := io.in(30, 23)
  valWire := Cat(1.U(1.W), io.in(22, 0))

  expi := (exp2.zext - 127.S + WOF.S).asSInt

  when(exp2.andR) {
    overflow := true.B
  }.elsewhen(io.in(30, 0) =/= 0.U) {
    for (ii <- 23 to 0 by -1) {
      when(valWire(ii)) {
        when(expi >= (WOI + WOF - 1).S) {
          overflow := true.B
        }.elsewhen(expi >= 0.S) {
          out := out.bitSet(expi.asUInt, true.B)
        }.elsewhen(ROUND.B && expi === -1.S) {
          round := true.B
        }
      }
      expi := expi - 1.S
    }
    when(round) {
      out := out + 1.U
    }
  }

  when(overflow) {
    when(sign) {
      out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
    }.otherwise {
      out := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
    }
  }.otherwise {
    when(sign) {
      out := (~out).asUInt + ONEO
    }
  }

  io.out := out
  io.overflow := overflow
}

class Float2FxpPipe(val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1) extends Module {
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
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Int = 1)
    extends Module {
  val io = IO(new Bundle {
    val ina = Input(UInt((WIIA + WIFA).W))
    val inb = Input(UInt((WIIB + WIFB).W))
    // true for subtraction, false for addition
    val sub = Input(common.AddOrSub())
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val WIIBE = WIIB + 1
  val WII = math.max(WIIA, WIIBE)
  val WIF = math.max(WIFA, WIFB)
  val WRI = WII + 1
  val WRF = WIF

  val ONE = 1.U((WIIBE + WIFB).W)
  val inbe = Wire(UInt((WIIBE + WIFB).W))
  val inaz = Wire(UInt((WII + WIF).W))
  val inbz = Wire(UInt((WII + WIF).W))
  val inbv = Mux(io.sub === common.AddOrSub.SUB, ~inbe + ONE, inbe)
  val res = (inaz.asSInt + inbz.asSInt).asUInt

  val inbExtend = Module(new FxpZoom(WIIB, WIFB, WIIBE, WIFB, 0))
  inbExtend.io.in := io.inb
  inbe := inbExtend.io.out

  val inaZoom = Module(new FxpZoom(WIIA, WIFA, WII, WIF, 0))
  inaZoom.io.in := io.ina
  inaz := inaZoom.io.out

  val inbZoom = Module(new FxpZoom(WIIBE, WIFB, WII, WIF, 0))
  inbZoom.io.in := inbv
  inbz := inbZoom.io.out

  val resZoom = Module(new FxpZoom(WRI, WRF, WOI, WOF, ROUND))
  resZoom.io.in := res
  io.out := resZoom.io.out
  io.overflow := resZoom.io.overflow
}

class FxpMulPipe(
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Int = 1)
    extends Module {
  val io = IO(new Bundle {
    val ina = Input(UInt((WIIA + WIFA).W))
    val inb = Input(UInt((WIIB + WIFB).W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val WRI = WIIA + WIIB
  val WRF = WIFA + WIFB

  val res = RegInit(0.S((WRI + WRF).W))
  val outc = WireDefault(0.U((WOI + WOF).W))
  val overflowc = WireDefault(false.B)

  res := io.ina.asSInt * io.inb.asSInt

  val resZoom = Module(new FxpZoom(WRI, WRF, WOI, WOF, ROUND))
  resZoom.io.in := res.asUInt
  outc := resZoom.io.out
  overflowc := resZoom.io.overflow

  io.out := RegNext(outc)
  io.overflow := RegNext(overflowc)

}

// pipeline stage = WOI + WOF + 3
class FxpDivPipe(
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Int = 1)
    extends Module {
  val io = IO(new Bundle {
    val dividend = Input(Valid(UInt((WIIA + WIFA).W)))
    val divisor = Input(Valid(UInt((WIIB + WIFB).W)))
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  val WRI = if (WOI + WIIB > WIIA) WOI + WIIB else WIIA
  val WRF = if (WOF + WIFB > WIFA) WOF + WIFB else WIFA

  val divd = Wire(UInt((WRI + WRF).W))
  val divr = Wire(UInt((WRI + WRF).W))
  val roundedres = RegInit(0.U((WOI + WOF).W))
  val rsign = RegInit(false.B)
  val sign = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val acc = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val divdp = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val divrp = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val validPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val res = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WOI + WOF).W))))

  val ONEO = 1.U((WOI + WOF).W)
  val ONEA = 1.U((WIIA + WIFA).W)
  val ONEB = 1.U((WIIB + WIFB).W)

  // Convert dividend and divisor to positive numbers
  val udividend = Mux(io.dividend.bits(WIIA + WIFA - 1), (~io.dividend.bits) + ONEA, io.dividend)
  val udivisor = Mux(io.divisor.bits(WIIB + WIFB - 1), (~io.divisor.bits) + ONEB, io.divisor)

  val dividendZoom = Module(new FxpZoom(WIIA, WIFA, WRI, WRF, 0))
  val divisorZoom = Module(new FxpZoom(WIIB, WIFB, WRI, WRF, 0))
  dividendZoom.io.in := udividend
  divd := dividendZoom.io.out

  divisorZoom.io.in := udivisor
  divr := divisorZoom.io.out

  val tmp = RegInit(0.U((WRI + WRF).W))
  // 1st pipeline stage: convert dividend and divisor to positive numbers
  acc(0) := 0.U
  divdp(0) := divd
  divrp(0) := divr
  validPipe(0) := io.dividend.valid && io.divisor.valid
  io.out.valid := ShiftRegister(validPipe(WOI + WOF), 2)
  sign(0) := io.dividend.bits(WIIA + WIFA - 1) ^ io.divisor.bits(WIIB + WIFB - 1)

  // From 2nd to WOI + WOF + 1 pipeline stages: calculate division
  for (ii <- 0 until WOI + WOF) {
    res(ii + 1) := res(ii)
    divdp(ii + 1) := divdp(ii)
    divrp(ii + 1) := divrp(ii)
    sign(ii + 1) := sign(ii)
    validPipe(ii + 1) := validPipe(ii)
    if (ii < WOI) tmp := acc(ii) + (divrp(ii) << (WOI - 1 - ii))
    else tmp := acc(ii) + (divrp(ii) >> (1 + ii - WOI))
    when(tmp < divdp(ii)) {
      acc(ii + 1) := tmp
      res(ii + 1)(WOF + WOI - 1 - ii) := true.B
    }.otherwise {
      acc(ii + 1) := acc(ii)
      res(ii + 1)(WOF + WOI - 1 - ii) := false.B
    }
  }

  // Next pipeline stage: process round
  when(
    ROUND.B && !res(WOI + WOF).andR && (acc(WOI + WOF) + (divrp(WOI + WOF) >> WOF) - divdp(WOI + WOF)) < (divdp(
      WOI + WOF
    ) - acc(WOI + WOF))
  ) {
    roundedres := res(WOI + WOF) + ONEO
  }.otherwise {
    roundedres := res(WOI + WOF)
  }
  rsign := sign(WOI + WOF)

  // The last pipeline stage: process roof and output
  io.overflow := false.B
  when(rsign) {
    when(roundedres(WOI + WOF - 1)) {
      io.overflow := Mux(roundedres(WOI + WOF - 2, 0).orR, true.B, false.B)
      io.out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
    }.otherwise {
      io.out.bits := (~roundedres) + ONEO
    }
  }.otherwise {
    when(roundedres(WOI + WOF - 1)) {
      io.overflow := true.B
      io.out.bits := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
    }.otherwise {
      io.out.bits := roundedres
    }
  }
}
