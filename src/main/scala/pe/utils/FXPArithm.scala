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
