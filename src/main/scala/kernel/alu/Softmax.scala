package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fixedpoint._
import kernel.utils.PipeValue
import math.pow
import kernel.utils.common

trait SoftmaxAccuracy {
  val I: Int = 8
  val F: Int = 24
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
    // val x = Input(Valid(Vec(arraySize, UInt((I + F).W))))
    val x = Flipped(Decoupled(Vec(arraySize, UInt((I + F).W))))
    val soft_x = Decoupled(Vec(arraySize, UInt((I + F).W)))
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

// from Sanger, need test
class ExpUnitFixPoint(width: Int, point: Int, lut_bits: Int, append_bits: Int) extends Module {
  val v_width = width + append_bits
  val v_point = point + append_bits
  val fpType = FixedPoint(width.W, point.BP)
  val vType = FixedPoint(v_width.W, v_point.BP)
  val io = IO(new Bundle {
    val in_value = Input(fpType)
    val out_exp = Output(fpType)
  })

  val x = Wire(UInt(width.W))
  val y = Wire(UInt(v_width.W))
  val z1 = Wire(vType)
  val z2 = Wire(vType)

  val s = Reg(fpType)

  val u = Wire(UInt((width - point).W))
  val v = Wire(vType)

  val testers =
    Range.BigDecimal(0.0, 1.0, pow(2.0, -point)).map((a) => pow(2.0, a.toDouble) - a)
  val d_value =
    (testers.reduce((a, b) => if (a > b) a else b) +
      testers.reduce((a, b) => if (a < b) a else b)) / 2.0

  val d_fixed = FixedPoint.fromBigDecimal(d_value, v_width.W, v_point.BP)
  val d_wire = Wire(vType)
  if (lut_bits == 0)
    d_wire := d_fixed
  else {
    val lut_in = Range(0, 1 << lut_bits)
    val lut_out =
      lut_in
        .map((x) => x / pow(2.0, lut_bits))
        .map((x) => {
          val r = Range
            .BigDecimal(x, x + pow(2.0, -lut_bits), pow(2.0, -lut_bits))
            .map((y) => pow(2.0, y.toDouble) - y)
          (r.reduce((a, b) => if (a > b) a else b) +
            r.reduce((a, b) => if (a < b) a else b)) / 2.0
        })
        .map((x) =>
          FixedPoint
            .fromBigDecimal(x, v_width.W, v_point.BP)
        )
    // val lut_mem = Mem(lut_in.length, vType)
    // for (i <- 0 until lut_out.length)
    //   lut_mem(i.U) := lut_out(i)

    val v_bits = Wire(UInt(lut_bits.W))
    v_bits := v.asUInt(v_point - 1, v_point - lut_bits)

    var w = when(v_bits === lut_in(0).U) {
      d_wire := lut_out(0)
    }
    for (i <- 1 until lut_in.size)
      w = w.elsewhen(v_bits === lut_in(i).U) {
        d_wire := lut_out(i)
      }
    w.otherwise {
      d_wire := DontCare
    }
    // d_wire := lut_mem(v_bits)
  }
  // println(d_fixed)

  x := io.in_value.asUInt
  y := (x << append_bits) + (x << (append_bits - 1)) - (x << (append_bits - 4));

  u := y(v_width - 1, v_point)
  v := Cat(0.U((v_width - v_point).W), y(v_point - 1, 0))
    .asFixedPoint(v_point.BP)

  z1 := v + d_wire
  z2 := z1 << u;

  // printf(
  //   "x:%b y:%b u:%b v:%b d:%b z1:%b z2:%b\n",
  //   x,
  //   y,
  //   u,
  //   v.asUInt(),
  //   d_wire.asUInt(),
  //   z1.asUInt(),
  //   z2.asUInt()
  // )

  io.out_exp := z2
}
