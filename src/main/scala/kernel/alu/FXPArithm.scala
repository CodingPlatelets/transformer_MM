package kernel.alu

import chisel3._
import chisel3.util._
import fixedpoint._
import kernel.utils.DebugLog

// test pass
class FxpZoom(val WII: Int = 8, val WIF: Int = 8, val WOI: Int = 8, val WOF: Int = 8, val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val in = Input(Valid(UInt((WII + WIF).W)))
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  val inr = Reg(UInt((WII + WOF).W))
  val ini = inr(WII + WOF - 1, WOF)
  val outf = Reg(UInt(WOF.W))
  val outi = Reg(UInt(WOI.W))
  val overflow = RegInit(false.B)

  val zoomLatency = 2

  io.out.valid := ShiftRegister(io.in.valid, zoomLatency)
  io.overflow.valid := ShiftRegister(io.in.valid, zoomLatency)

  // BP flow
  if (WOF > WIF) {
    inr := Cat(io.in.bits, Fill(WOF - WIF, 0.U))
  } else if (WOF == WIF || !ROUND) {
    inr := io.in.bits(WII + WIF - 1, 0)
  } else {
    val tempInr = io.in.bits(WII + WIF - 1, (WIF - WOF).abs)
    if (WII + WOF >= 2) {
      when(io.in.bits(WIF - WOF - 1) && ~(~tempInr(WII + WOF - 1) && (tempInr(WII + WOF - 2, 0).andR))) {
        inr := tempInr +& 1.U
      }.otherwise {
        inr := tempInr
      }
    } else {
      when(io.in.bits(WIF - WOF - 1) && tempInr(WII + WOF - 1)) {
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
    outf := inr(WOF - 1, 0)
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

  io.out.bits := Cat(outi, outf)
  io.overflow.bits := overflow

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

// test pass
class Fxp2FloatPipe(val WII: Int = 8, val WIF: Int = 8) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in = Input(Valid(UInt((WII + WIF).W)))
    val out = Output(Valid(UInt(32.W)))
  })

  io.out.valid := ShiftRegister(io.in.valid, WIF + WII + 2)
  val ONEI = 1.U((WII + WIF).W)

  val signPipe = RegInit(VecInit.fill(WII + WIF + 1)(false.B))
  val expPipe = RegInit(VecInit.fill(WII + WIF + 1)(0.U(10.W)))
  val inuPipe = RegInit(VecInit.fill(WII + WIF + 1)(0.U((WII + WIF).W)))

  val vall = WireDefault(0.U(24.W))
  val valo = RegInit(0.U(24.W))
  val expo = RegInit(0.U(8.W))
  val signo = RegInit(false.B)

  // init
  val inputSign = io.in.bits
  signPipe(WII + WIF) := inputSign(WII + WIF - 1).asBool
  expPipe(WII + WIF) := (WII + 127 - 1).U
  inuPipe(WII + WIF) := Mux(inputSign(WII + WIF - 1).asBool, ~inputSign + ONEI, inputSign)

  // pipeline stages
  for (ii <- (WII + WIF - 1) to 0 by -1) {
    signPipe(ii) := signPipe(ii + 1)
    when(inuPipe(ii + 1)(WII + WIF - 1)) {
      expPipe(ii) := expPipe(ii + 1)
      inuPipe(ii) := inuPipe(ii + 1)
    }.otherwise {
      inuPipe(ii) := (inuPipe(ii + 1) << 1)
      when(expPipe(ii + 1) =/= 0.U) {
        expPipe(ii) := expPipe(ii + 1) - 1.U
      }.otherwise {
        expPipe(ii) := expPipe(ii + 1)
      }
    }
  }

  if (23 > WII + WIF - 1) {
    vall := Cat(inuPipe(0), 0.U)
  } else {
    vall := inuPipe(0)(WII + WIF - 1, WII + WIF - 1 - 23)
  }

  when(expPipe(0) >= 255.U(10.W)) {
    expo := 255.U(8.W)
    valo := "hFFFFFF".U
    signo := signPipe(0)
  }.elsewhen(expPipe(0) === 0.U || !vall(23)) {
    expo := 0.U(8.W)
    valo := 0.U
    signo := signPipe(0)
  }.otherwise {
    expo := expPipe(0)(7, 0).asUInt
    valo := vall
    signo := signPipe(0)
  }

  io.out.bits := Cat(signo, expo, valo(22, 0))
}

// test pass
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

// test pass
class Float2FxpPipe(val WOI: Int = 8, val WOF: Int = 8, val ROUND: Int = 1) extends Module with DebugLog {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt((WOI + WOF).W))
    val overflow = Output(Bool())
  })

  val ONEO = 1.U((WOI + WOF).W)

  // Input comb
  val sign = io.in(31)
  val exp = io.in(30, 23)
  val valWire = Cat(exp.orR, io.in(22, 0))

  // Pipeline stage 1
  val signinit = RegInit(false.B)
  val roundinit = RegInit(false.B)
  val expinit = RegInit(0.S(32.W))
  val outinit = RegInit(0.U((WOI + WOF).W))

  if (WOI + WOF - 1 >= 23) {
    outinit := Cat(valWire, 0.U((WOI + WOF - 1 - 23).W))
    roundinit := false.B
  } else {
    outinit := valWire(23, 23 - (WOI + WOF - 1))
    roundinit := (ROUND.B && valWire(23 - (WOI + WOF - 1) - 1))
  }

  signinit := sign
  when(exp === 255.U || Cat(0.U(24.W), exp) > (WOI + 126).U) {
    expinit := 0.S
  }.otherwise {
    expinit := Cat(0.U(24.W), exp).asSInt - (WOI - 1).S - 127.S
  }

  // Next pipeline stages
  val signs = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val rounds = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val exps = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U(32.W))))
  val outs = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WOI + WOF).W))))

  // debugLog(
  //   p"io.in: ${io.in}, signinit: ${signinit}, roundinit: ${roundinit}, expinit: ${expinit}, outinit: ${outinit}.\n \n",
  //   LogLevel.DEBUG
  // )

  // init
  signs(WOI + WOF) := signinit
  rounds(WOI + WOF) := roundinit
  exps(WOI + WOF) := expinit.asUInt
  outs(WOI + WOF) := outinit

  // stage 1
  for (ii <- 0 until WOI + WOF) {
    signs(ii) := signs(ii + 1)
    when(exps(ii + 1) =/= 0.U) {
      outs(ii) := Cat(0.U(1.W), outs(ii + 1)(WOI + WOF - 1, 1))
      rounds(ii) := outs(ii + 1)(0)
      exps(ii) := exps(ii + 1) + 1.U
    }.otherwise {
      outs(ii) := outs(ii + 1)
      rounds(ii) := rounds(ii + 1)
      exps(ii) := exps(ii + 1)
    }
  }
  // debugLog(p"signs: ${signs},\n rounds: ${rounds},\n exps: ${exps},\n outs: ${outs}.\n \n", LogLevel.DEBUG)

  // Last 2nd pipeline stage
  val signl = RegInit(false.B)
  val outl = RegInit(0.U((WOI + WOF).W))

  when(signs(0)) {
    outl := Mux(ROUND.B && rounds(0) && !outs(0).andR, ~(outs(0) + 1.U) + ONEO, ~outs(0) + ONEO)
    signl := Mux(ROUND.B && rounds(0) && !outs(0).andR, true.B, outs(0) =/= 0.U)
  }.otherwise {
    outl := outs(0)
    signl := false.B
  }

  // Last 1st pipeline stage: overflow control
  when(signl) {
    when(!outl(WOI + WOF - 1)) {
      io.out := Cat(1.U(1.W), 0.U((WOI + WOF - 1).W))
      io.overflow := true.B
    }.otherwise {
      io.out := outl
      io.overflow := false.B
    }
  }.otherwise {
    when(outl(WOI + WOF - 1)) {
      io.out := Cat(0.U(1.W), Fill(WOI + WOF - 1, 1.U))
      io.overflow := true.B
    }.otherwise {
      io.out := outl
      io.overflow := false.B
    }
  }
}

class FxpAdd(
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val ina = Input(Valid(UInt((WIIA + WIFA).W)))
    val inb = Input(Valid(UInt((WIIB + WIFB).W)))
    // true for subtraction, false for addition
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  val WII = if (WIIA > WIIB + 1) WIIA else WIIB + 1
  val WIF = if (WIFA > WIFB) WIFA else WIFB

  val middleWire = Wire(FixedPoint((WII + WIF + 1).W, WIF.BP))

  middleWire := io.ina.bits.asFixedPoint(WIFA.BP) +& io.inb.bits.asFixedPoint(WIFB.BP)

  val resZoom = Module(new FxpZoom(WII + 1, WIF, WOI, WOF, ROUND))
  val addLatency = 0
  resZoom.io.in <> Pipe(io.ina.valid && io.inb.valid, middleWire.asUInt, addLatency)
  io.out <> resZoom.io.out
  io.overflow <> resZoom.io.overflow

}

class FxpMul(
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val ina = Input(Valid(UInt((WIIA + WIFA).W)))
    val inb = Input(Valid(UInt((WIIB + WIFB).W)))
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  val WRI = WIIA + WIIB
  val WRF = WIFA + WIFB

  val middleWire = Wire(FixedPoint((WRI + WRF).W, WRF.BP))
  middleWire := io.ina.bits.asFixedPoint(WIFA.BP) * io.inb.bits.asFixedPoint(WIFB.BP)

  val resZoom = Module(new FxpZoom(WRI, WRF, WOI, WOF, ROUND))
  val mulLatency = 0
  resZoom.io.in <> Pipe(io.ina.valid && io.inb.valid, middleWire.asUInt, mulLatency)
  io.out <> resZoom.io.out
  io.overflow <> resZoom.io.overflow
}

class FxpDiv(
  val WIIA:  Int = 8,
  val WIFA:  Int = 8,
  val WIIB:  Int = 8,
  val WIFB:  Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val dividend = Input(Valid(UInt((WIIA + WIFA).W)))
    val divisor = Input(Valid(UInt((WIIB + WIFB).W)))
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  val WRI = if (WOI + WIIB > WIIA) WOI + WIIB else WIIA
  val WRF = if (WOF + WIFB > WIFA) WOF + WIFB else WIFA

  val ONEA = 1.U((WIIA + WIFA).W)
  val ONEB = 1.U((WIIA + WIFA).W)

  val divendZoom = Module(new FxpZoom(WIIA, WIFA, WRI, WRF, false))
  val divorZoom = Module(new FxpZoom(WIIB, WIFB, WRI, WRF, false))

  val divd = Wire(UInt((WRI + WRF).W))
  val divr = Wire(UInt((WRI + WRF).W))

  // convert both dividend and divisor to postive number and the same width
  divendZoom.io.in <> Pipe(
    io.dividend.valid,
    Mux(io.dividend.bits(WIIA + WIFA - 1), (~io.dividend.bits) + ONEA, io.dividend.bits),
    0
  )
  divorZoom.io.in <> Pipe(
    io.divisor.valid,
    Mux(io.divisor.bits(WIIB + WIFB - 1), (~io.divisor.bits) + ONEB, io.divisor.bits),
    0
  )

  divendZoom.io.overflow := DontCare
  divorZoom.io.overflow := DontCare

  divd := divendZoom.io.out.bits
  divr := divorZoom.io.out.bits
  val divLatency = WOI + WOF + 3
  io.out.valid := ShiftRegister(divendZoom.io.out.valid && divorZoom.io.out.valid, divLatency)
  io.overflow.valid := ShiftRegister(divendZoom.io.out.valid && divorZoom.io.out.valid, divLatency)

  val signPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(false.B)))
  val accPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val divdPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val divrPipe = RegInit(VecInit(Seq.fill(WOI + WOF + 1)(0.U((WRI + WRF).W))))
  val resPipe = RegInit(VecInit.fill(WOI + WOF + 1, WOI + WOF)(false.B))
  val ONEO = 1.U((WOI + WOF).W)

  // init the first stage
  resPipe(0) := VecInit.fill(WOI + WOF)(false.B)
  accPipe(0) := 0.U
  divdPipe(0) := divd
  divrPipe(0) := divr
  signPipe(0) := ShiftRegister(io.dividend.bits(WIIA + WIFA - 1) ^ io.divisor.bits(WIIB + WIFB - 1), 2)

  // pipeline stages
  for (ii <- 0 until WOI + WOF) {
    resPipe(ii + 1) := resPipe(ii)
    divdPipe(ii + 1) := divdPipe(ii)
    divrPipe(ii + 1) := divrPipe(ii)
    signPipe(ii + 1) := signPipe(ii)

    val temp =
      if (ii < WOI) {
        accPipe(ii) + (divrPipe(ii) << (WOI - 1 - ii))
      } else {
        accPipe(ii) + (divrPipe(ii) >> (1 + ii - WOI))
      }

    when(temp < divdPipe(ii)) {
      accPipe(ii + 1) := temp
      resPipe(ii + 1)(WOF + WOI - 1 - ii) := true.B
    }.otherwise {
      accPipe(ii + 1) := accPipe(ii)
      resPipe(ii + 1)(WOF + WOI - 1 - ii) := false.B
    }
  }

  // process ROUND
  val rounder = RegInit(0.U((WOI + WOF).W))
  val rSign = RegInit(false.B)
  val lastPipeBigger =
    accPipe(WOI + WOF) + (divrPipe(WOI + WOF) >> WOF) - divdPipe(WOI + WOF) < divdPipe(WOI + WOF) - accPipe(WOI + WOF)
  when(ROUND.B && !(resPipe(WOI + WOF).asUInt.andR) && lastPipeBigger) {
    rounder := resPipe(WOI + WOF).asUInt + ONEO
  }.otherwise {
    rounder := resPipe(WOI + WOF).asUInt
  }
  rSign := signPipe(WOI + WOF)

  // process root and output
  val overflow = RegInit(false.B)
  val res = RegInit(0.U((WOI + WOF).W))

  when(rSign) {
    when(rounder(WOI + WOF - 1)) {
      when(rounder(WOI + WOF - 2, 0).orR) {
        overflow := true.B
      }.otherwise(overflow := false.B)
      res := 1.U ## Fill(WOI + WOF - 1, 0.U)
    }.otherwise {
      res := (~rounder) + ONEO
      overflow := false.B
    }
  }.otherwise {
    when(rounder(WOI + WOF - 1)) {
      overflow := true.B
      res := 0.U ## Fill(WOI + WOF - 1, 1.U)
    }.otherwise {
      res := rounder
      overflow := false.B
    }
  }
  io.overflow.bits <> overflow
  io.out.bits <> res
}

class FxpSqrt(
  val WII:   Int = 8,
  val WIF:   Int = 8,
  val WOI:   Int = 8,
  val WOF:   Int = 8,
  val ROUND: Boolean = true)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val in = Input(Valid(UInt((WII + WIF).W)))
    val out = Valid(UInt((WOI + WOF).W))
    val overflow = Valid(Bool())
  })

  var WTI = if (WII % 2 == 1) WII + 1 else WII
  val WRI = WTI / 2

  val ONET = 1.U((WTI + WIF - 1).W)
  val ONER = 1.U((WRI + WIF - 1).W)

  val signPipe = RegInit(VecInit.fill(WRI + WIF + 1)(false.B))
  val inuPipe = RegInit(VecInit.fill(WRI + WIF + 1)(0.U((WTI + WIF).W)))
  val resu2Pipe = RegInit(VecInit.fill(WRI + WIF + 1)(0.U((WTI + WIF).W)))
  val resuPipe = RegInit(VecInit.fill(WRI + WIF + 1)(0.U((WTI + WIF).W)))

  // init the first stage
  signPipe(0) := io.in.bits(WII + WIF - 1)
  val inputPostive = io.in.bits.asSInt.pad(WTI + WIF)
  inuPipe(0) := Mux(inputPostive < 0.S, -inputPostive, inputPostive).asUInt
  resu2Pipe(0) := 0.U
  resuPipe(0) := 0.U

  // pipeline stages
  for (ii <- WRI - 1 to -WIF by -1) {
    val jj = WRI - 1 - ii
    signPipe(jj + 1) := signPipe(jj)
    inuPipe(jj + 1) := inuPipe(jj)

    val resu2Tmp = if (ii >= 0) {
      resu2Pipe(jj) +& (resuPipe(jj) << (1 + ii)) +& (ONET << (2 * ii + WIF))
    } else if (2 * ii + WIF >= 0) {
      resu2Pipe(jj) +& (resuPipe(jj) >> (-1 - ii)) +& (ONET << (2 * ii + WIF))
    } else {
      resu2Pipe(jj) +& (resuPipe(jj) >> (-1 - ii))
    }

    when(resu2Tmp <= inuPipe(jj) && inuPipe(jj) =/= 0.U) {
      resuPipe(jj + 1) := (1.U(1.W) << (WIF + ii)) | resuPipe(jj)
      resu2Pipe(jj + 1) := resu2Tmp
    }.otherwise {
      resu2Pipe(jj + 1) := resu2Pipe(jj)
      resuPipe(jj + 1) := resuPipe(jj)
    }

  }

  // process ROUND
  val resushort =
    Mux(
      signPipe(WRI + WIF),
      ~(resuPipe(WRI + WIF)(WRI + WIF, 0)) + ONER,
      resuPipe(WRI + WIF)(WRI + WIF, 0)
    )

  val resZoom = Module(new FxpZoom(WRI + 1, WIF, WOI, WOF, ROUND))

  val sqrtLatency = (WII + 2 - 1) / 2 + WIF + 1
  resZoom.io.in.valid := ShiftRegister(io.in.valid, sqrtLatency)

  resZoom.io.in.bits := resushort
  io.out <> resZoom.io.out
  io.overflow <> resZoom.io.overflow

  // debugLog(
  //   p"in: ${io.in}, signPipe: ${signPipe}, \n inuPipe: ${inuPipe},\n resu2Pipe: ${resu2Pipe}, \n resuPipe: ${resuPipe},\n resushort: ${resushort}\n",
  //   LogLevel.DEBUG
  // )
}
