package pe.utils

import chisel3._
import chisel3.util._
import pe.configs.SdpmmConfigs
import pe.utils.DebugLog
import fixedpoint._
import pe.utils.PipeValue
import pe.utils.common
import coursier.core.Version.Min

// class Softmax extends Module with DebugLog {

//   val InputPipe = IO(
//     Flipped(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))
//   )
//   val OutputPipe = IO(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))
//   OutputPipe := DontCare

//   val InputQueue = Module(
//     new Queue(
//       new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask),
//       SdpmmConfigs.queueSize,
//       pipe = false,
//       flow = false,
//       useSyncReadMem = false
//     )
//   )

//   InputQueue.io.deq := DontCare
//   InputQueue.io.enq <> InputPipe

//   val ready = RegInit(true.B)
//   InputQueue.io.deq.ready := ready
//   val valid = RegInit(false.B)
//   OutputPipe.valid := valid
//   val wholeWidth = SdpmmConfigs.bit + SdpmmConfigs.fixedPoint
//   val expALUs =
//     for (i <- 0 until SdpmmConfigs.dim)
//       yield Module(new FixedPointExp(wholeWidth, SdpmmConfigs.fixedPoint))
//   val numsOri = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.S(wholeWidth.W))))
//   val numsExp = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.U(wholeWidth.W))))
//   val tempMasks = RegInit(VecInit(Seq.fill(SdpmmConfigs.numOfMask)(0.U(common.maskType.W))))
//   OutputPipe.bits.mask := tempMasks

//   //todo: sub  max

//   // // each expALU.io.x is each element of numsOri
//   // for (i <- 0 until SdpmmConfigs.dim) {
//   //   expALUs(i).io.x := Cat(0.U(SdpmmConfigs.fixedPoint.W), numsOri(i))
//   // }
//   for (i <- expALUs) {
//     i.io := DontCare
//   }

//   val sumExp = RegInit(0.U((SdpmmConfigs.bit).W))

//   object State extends ChiselEnum {
//     val sIdle, sMax, sExp, sSum, sDiv = Value
//   }

//   val state = RegInit(State.sIdle)
//   val max = numsOri.reduceTree((a, b) => Mux(a > b, a, b))

//   debugLog(
//     p"state: ${state}\n" +
//       p"numOri: ${numsOri}\n" +
//       p"max: ${max}\n" +
//       p"numExp: ${numsExp}\n" +
//       p"sumExp: ${sumExp}\n" +
//       p"valid: ${valid}\n" +
//       p"ready: ${ready}\n" +
//       p"mask: ${tempMasks}\n" +
//       p"\n"
//   )

//   switch(state) {
//     is(State.sIdle) {
//       when(InputQueue.io.deq.valid) {
//         ready := false.B
//         tempMasks := InputQueue.io.deq.bits.mask
//         numsOri := InputQueue.io.deq.bits.value.map(x => Cat(0.U(SdpmmConfigs.fixedPoint.W), x).asSInt)
//         state := State.sMax
//       }
//     }
//     is(State.sMax) {
//       numsOri := numsOri.map(_ -& max)
//       state := State.sExp
//     }

//     is(State.sExp) {
//       for ((e, n) <- expALUs.zip(numsOri)) {
//         e.io.x := n
//       }

//       numsExp := expALUs.map(x => x.io.exp_x)
//       state := State.sSum
//     }

//     is(State.sSum) {
//       sumExp := numsExp.reduceTree(_ +& _)
//       state := State.sDiv
//     }

//     is(State.sDiv) {
//       val exps = WireDefault(VecInit(numsExp.map(_ / sumExp).map(x => x(SdpmmConfigs.bit - 1, 0).asUInt)))
//       OutputPipe.bits.value := exps
//       valid := true.B
//       when(valid && OutputPipe.ready) {
//         ready := true.B
//         valid := false.B
//         state := State.sIdle
//       }
//     }
//   }

// }

class FixedPointExp(val wholeWidth: Int, val fractionalWidth: Int) extends Module with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(SInt((wholeWidth).W)))
    val exp_x = Valid(UInt((wholeWidth).W))
  })

  val z = Wire(SInt(((wholeWidth).W)))
  val p = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val lp = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val ln2 = WireDefault(FixedPoint.fromBigDecimal(0.6931471805599453, wholeWidth.W, fractionalWidth.BP))
  val bias1 = WireDefault(FixedPoint.fromBigDecimal(1.353, wholeWidth.W, fractionalWidth.BP))
  val k1 = WireDefault(FixedPoint.fromBigDecimal(0.3585, wholeWidth.W, fractionalWidth.BP))
  val bias2 = WireDefault(FixedPoint.fromBigDecimal(0.344, wholeWidth.W, fractionalWidth.BP))

  val expDelay = 3

  // z = floor[x/ln2]
  z := io.x.bits.abs / ln2.asSInt

  val z_delay = RegNext(z)
  val z_delay2 = RegNext(z_delay)

  // p = x + z * ln2
  // p := io.x.asFixedPoint(fractionalWidth.BP) + z.asFixedPoint(fractionalWidth.BP) * ln2
  p := (RegNext(io.x.bits) + z_delay * ln2.asUInt).asFixedPoint(fractionalWidth.BP)

  lp := RegNext(k1 * (p + bias1) * (p + bias1) + bias2)
  io.exp_x.bits := RegNext(lp >> z_delay2.asUInt).asUInt
  io.exp_x.valid := ShiftRegister(io.x.valid, expDelay)
}

class Softmax(val WII: Int, val WIF: Int, val WOI: Int, val WOF: Int, val arraySize: Int = 4)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(Vec(arraySize, UInt((WII + WIF).W))))
    val soft_x = Valid(Vec(arraySize, UInt((WOI + WOF).W)))
  })

  // first find the max value of x
  val max = RegInit(0.U((WII + WIF).W))
  max := io.x.bits.reduceTree((a, b) => Mux(a > b, a, b))

  // then find all the exp(x - max)
  val expX = io.x.bits.map { x =>
    val expALU = Module(new FixedPointExp(WII + WIF, WOF))
    expALU.io.x.bits := x - max
    expALU.io.x.valid := io.x.valid
    expALU.io.exp_x
  }

  val exp_sum = Pipe(expX.map(_.valid).reduce(_ & _), VecInit(expX.map(_.bits)).reduceTree(_ +& _), 0)

  // finally divide each exp(x - max) by exp_sum
  val softTmp = expX.map { exp =>
    val divModule = Module(new FxpDiv(WII + WIF, WOF, WOI, WOF))
    divModule.io.dividend := exp
    divModule.io.divisor := exp_sum
    divModule.io.out
  }

  io.soft_x <> Pipe(softTmp.map(_.valid).reduce(_ & _), VecInit(softTmp.map(_.bits)), 0)
}
