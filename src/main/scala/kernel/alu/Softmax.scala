package kernel.alu

import chisel3._
import chisel3.util._
import kernel.configs.SdpmmConfigs
import kernel.utils.DebugLog
import fixedpoint._
import kernel.utils.PipeValue
import kernel.utils.common
import coursier.core.Version.Min

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
