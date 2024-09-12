package pe

import chisel3._
import chisel3.util._
import configs.SdpmmConfigs
import vitiskernel.util.DebugLog
import fixedpoint._
import pe.utils.PipeValue
import pe.utils.common
import coursier.core.Version.Min

class Softmax extends Module with DebugLog {

  val InputPipe = IO(
    Flipped(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))
  )
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))
  OutputPipe := DontCare

  val InputQueue = Module(
    new Queue(
      new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask),
      SdpmmConfigs.queueSize,
      pipe = false,
      flow = false,
      useSyncReadMem = false
    )
  )

  InputQueue.io.deq := DontCare
  InputQueue.io.enq <> InputPipe

  val ready = RegInit(true.B)
  InputQueue.io.deq.ready := ready
  val valid = RegInit(false.B)
  OutputPipe.valid := valid
  val wholeWidth = SdpmmConfigs.bit + SdpmmConfigs.fixedPoint
  val expALUs =
    for (i <- 0 until SdpmmConfigs.dim)
      yield Module(new FixedPointExp(wholeWidth, SdpmmConfigs.fixedPoint))
  val numsOri = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.S(wholeWidth.W))))
  val numsExp = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.U(wholeWidth.W))))
  val tempMasks = RegInit(VecInit(Seq.fill(SdpmmConfigs.numOfMask)(0.U(common.maskType.W))))
  OutputPipe.bits.mask := tempMasks

  //todo: sub  max

  // // each expALU.io.x is each element of numsOri
  // for (i <- 0 until SdpmmConfigs.dim) {
  //   expALUs(i).io.x := Cat(0.U(SdpmmConfigs.fixedPoint.W), numsOri(i))
  // }
  for (i <- expALUs) {
    i.io := DontCare
  }

  val sumExp = RegInit(0.U((SdpmmConfigs.bit).W))

  object State extends ChiselEnum {
    val sIdle, sMax, sExp, sSum, sDiv = Value
  }

  val state = RegInit(State.sIdle)
  val max = numsOri.reduceTree((a, b) => Mux(a > b, a, b))

  debugLog(
    p"state: ${state}\n" +
      p"numOri: ${numsOri}\n" +
      p"max: ${max}\n" +
      p"numExp: ${numsExp}\n" +
      p"sumExp: ${sumExp}\n" +
      p"valid: ${valid}\n" +
      p"ready: ${ready}\n" +
      p"mask: ${tempMasks}\n" +
      p"\n"
  )

  switch(state) {
    is(State.sIdle) {
      when(InputQueue.io.deq.valid) {
        ready := false.B
        tempMasks := InputQueue.io.deq.bits.mask
        numsOri := InputQueue.io.deq.bits.value.map(x => Cat(0.U(SdpmmConfigs.fixedPoint.W), x).asSInt)
        state := State.sMax
      }
    }
    is(State.sMax) {
      numsOri := numsOri.map(_ -& max)
      state := State.sExp
    }

    is(State.sExp) {
      for ((e, n) <- expALUs.zip(numsOri)) {
        e.io.x := n
      }

      numsExp := expALUs.map(x => x.io.exp_x)
      state := State.sSum
    }

    is(State.sSum) {
      sumExp := numsExp.reduceTree(_ +& _)
      state := State.sDiv
    }

    is(State.sDiv) {
      val exps = WireDefault(VecInit(numsExp.map(_ / sumExp).map(x => x(SdpmmConfigs.bit - 1, 0).asUInt)))
      OutputPipe.bits.value := exps
      valid := true.B
      when(valid && OutputPipe.ready) {
        ready := true.B
        valid := false.B
        state := State.sIdle
      }
    }
  }

}

class FixedPointExp(val wholeWidth: Int, val fractionalWidth: Int) extends Module with DebugLog {
  val io = IO(new Bundle {
    val x = Input(SInt((wholeWidth).W))
    val exp_x = Output(UInt((wholeWidth).W))
  })

  val z = Wire(SInt(((wholeWidth).W)))
  val p = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val lp = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val ln2 = WireDefault(FixedPoint.fromBigDecimal(0.6931471805599453, wholeWidth.W, fractionalWidth.BP))
  val bias1 = WireDefault(FixedPoint.fromBigDecimal(1.353, wholeWidth.W, fractionalWidth.BP))
  val k1 = WireDefault(FixedPoint.fromBigDecimal(0.3585, wholeWidth.W, fractionalWidth.BP))
  val bias2 = WireDefault(FixedPoint.fromBigDecimal(0.344, wholeWidth.W, fractionalWidth.BP))

  // z = floor[x/ln2]
  z := io.x.abs / ln2.asSInt

  // p = x + z * ln2
  // p := io.x.asFixedPoint(fractionalWidth.BP) + z.asFixedPoint(fractionalWidth.BP) * ln2
  p := (io.x + z * ln2.asUInt).asFixedPoint(fractionalWidth.BP)

  val testp = FixedPoint.fromDouble(-2.1, wholeWidth.W, fractionalWidth.BP)

  lp := k1 * (p + bias1) * (p + bias1) + bias2
  io.exp_x := (lp >> z.asUInt).asUInt
}

// deprecated: just a UInt implementation of FixedPointExp
class ExpUInt(val wholeWidth: Int, val fractionalWidth: Int) extends Module with DebugLog {
  val io = IO(new Bundle {
    val x = Input(SInt((wholeWidth).W))
    val exp_x = Output(UInt((wholeWidth).W))
  })

  val uX = Wire(UInt((wholeWidth).W))
  uX := io.x.abs.asUInt

  val z = Wire(UInt(((wholeWidth).W)))
  val ln2 = WireDefault(FixedPoint.fromBigDecimal(0.6931471805599453, wholeWidth.W, fractionalWidth.BP))
  // z = floor[x/ln2]
  z := uX / ln2.asUInt
  val p = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  // -p = uX - z * ln2
  p := (uX - z * ln2.asUInt).asFixedPoint(fractionalWidth.BP)

  val lp = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val bias1 = WireDefault(FixedPoint.fromBigDecimal(1.353, wholeWidth.W, fractionalWidth.BP))
  val k1 = WireDefault(FixedPoint.fromBigDecimal(0.3585, wholeWidth.W, fractionalWidth.BP))
  val bias2 = WireDefault(FixedPoint.fromBigDecimal(0.344, wholeWidth.W, fractionalWidth.BP))

  // debugLog(p"uX: ${uX}\n")
  // debugLog(p"z: ${z}\n")
  // debugLog(p"z * ln2: ${z.asFixedPoint(fractionalWidth.BP) * ln2}\n")
  // debugLog(p"p.asUInt: ${p.asUInt}\n")
  lp := k1 * (bias1 - p) * (bias1 - p) + bias2
  // debugLog(p"lp: ${lp.asUInt}\n\n")
  io.exp_x := lp.asUInt >> z
}

// TODO: Input two UInts and output the result of division as UInt
class FixedPointDivision(val wholeWidth: Int, val fractionalWidth: Int) extends Module with DebugLog {

  val io = IO(new Bundle {
    val minuend = Input(UInt((wholeWidth).W))
    val subtrahend = Input(UInt((wholeWidth).W))
    val quotient = Output(UInt((wholeWidth).W))
  })
  assert(io.minuend >= io.subtrahend)

}
