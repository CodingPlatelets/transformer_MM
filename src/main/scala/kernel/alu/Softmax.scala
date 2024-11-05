package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fixedpoint._
import kernel.utils.PipeValue
import kernel.utils.common

trait SoftmaxAccuracy {
  val I: Int = 8
  val F: Int = 16
}

class FixedPointExp extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(SInt((I + F).W)))
    val exp_x = Valid(UInt((I + F).W))
  })

  val z = Wire(SInt((I + F).W))
  val p = Wire(FixedPoint((I + F).W, F.BP))
  val lp = Wire(FixedPoint((I + F).W, F.BP))
  val ln2 = WireDefault(FixedPoint.fromBigDecimal(0.6931471805599453, (I + F).W, F.BP))
  val bias1 = WireDefault(FixedPoint.fromBigDecimal(1.353, (I + F).W, F.BP))
  val k1 = WireDefault(FixedPoint.fromBigDecimal(0.3585, (I + F).W, F.BP))
  val bias2 = WireDefault(FixedPoint.fromBigDecimal(0.344, (I + F).W, F.BP))

  val expDelay = 3

  // z = floor[x/ln2]
  z := io.x.bits.abs / ln2.asSInt

  val z_delay = RegNext(z)
  val z_delay2 = RegNext(z_delay)

  // p = x + z * ln2
  // p := io.x.asFixedPoint(fractionalWidth.BP) + z.asFixedPoint(fractionalWidth.BP) * ln2
  p := (RegNext(io.x.bits) + z_delay * ln2.asUInt).asFixedPoint(F.BP)

  lp := RegNext(k1 * (p + bias1) * (p + bias1) + bias2)
  io.exp_x.bits := RegNext(lp >> z_delay2.asUInt).asUInt
  io.exp_x.valid := ShiftRegister(io.x.valid, expDelay)
}

class Softmax(val arraySize: Int = 4) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(Vec(arraySize, UInt((I + F).W))))
    val soft_x = Valid(Vec(arraySize, UInt((I + F).W)))
  })

  // first find the max value of x
  // cycle 1
  val max = RegInit(0.U((I + F).W))
  max := io.x.bits.reduceTree((a, b) => Mux(a > b, a, b))
  val xReg = RegNext(io.x.bits)

  // cycle 2
  // then find all the exp(x - max)
  val expX = xReg.map { x =>
    val expALU = Module(new FixedPointExp)
    expALU.io.x.bits := (~(max - x) + 1.U).asSInt
    expALU.io.x.valid := RegNext(io.x.valid)
    expALU.io.exp_x
  }

  val exp_sum = Pipe(expX.map(_.valid).reduce(_ & _), VecInit(expX.map(_.bits)).reduceTree(_ +& _), 0)

  // finally divide each exp(x - max) by exp_sum
  val softTmp = expX.map { exp =>
    val divModule = Module(new FxpDiv(I, F, I, F, I, F))
    divModule.io.dividend := exp
    divModule.io.divisor := exp_sum
    divModule.io.out
  }

  io.soft_x <> Pipe(softTmp.map(_.valid).reduce(_ & _), VecInit(softTmp.map(_.bits)), 0)
}
