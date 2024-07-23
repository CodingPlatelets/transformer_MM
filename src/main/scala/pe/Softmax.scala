package pe

import chisel3._
import chisel3.util._
import configs.SdpmmConfigs
import vitiskernel.util.DebugLog
import fixedpoint._
import pe.utils.PipeValue
import pe.utils.common

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

  InputQueue.io.enq <> InputPipe

  val ready = RegInit(true.B)
  InputQueue.io.deq.ready := ready
  val valid = RegInit(false.B)
  OutputPipe.valid := valid
  val expALUs =
    for (i <- 0 until SdpmmConfigs.dim) yield Module(new FixedPointExp(SdpmmConfigs.bit + 1, SdpmmConfigs.bit))
  val numsOri = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.S((SdpmmConfigs.bit + 1).W))))
  val numsExp = RegInit(VecInit(Seq.fill(SdpmmConfigs.dim)(0.S((SdpmmConfigs.bit + 1).W))))
  val tempMasks = RegInit(VecInit(Seq.fill(SdpmmConfigs.numOfMask)(0.U(common.maskType.W))))
  OutputPipe.bits.mask := tempMasks

  //todo: sub  max
  numsOri := InputQueue.io.deq.bits.value.map(x => x.zext)

  // each expALU.io.x is each element of numsOri
  for (i <- 0 until SdpmmConfigs.dim) {
    expALUs(i).io.x := numsOri(i)
  }

  numsExp := expALUs.map(x => x.io.exp_x)

  val sumExp = RegInit(0.S((SdpmmConfigs.bit + 1).W))

  object State extends ChiselEnum {
    val sIdle, sMax, sExp, sSum, sDiv = Value
  }

  val state = RegInit(State.sIdle)

  debugLog(
    p"state: ${state}\n" +
      p"numOri: ${numsOri}\n" +
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
        state := State.sMax
      }
    }
    is(State.sMax) {
      numsOri := numsOri.map(_ -& numsOri.reduceTree((a, b) => Mux(a > b, a, b)))
      state := State.sExp
    }
    is(State.sExp) {
      for (i <- 0 until SdpmmConfigs.dim) {
        expALUs(i).io.x := numsOri(i)
      }
      state := State.sSum
    }

    is(State.sSum) {
      sumExp := numsExp.reduceTree(_ +& _)
      state := State.sDiv
    }
    is(State.sDiv) {
      val exps = WireDefault(VecInit(numsExp.map(_ / sumExp).map(x => x.tail(SdpmmConfigs.bit))))
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
    val exp_x = Output(SInt((wholeWidth).W))
  })

  // z = floor(-x/log2)
  val z = Wire(SInt(((wholeWidth).W)))
  val p = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val lp = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val log2 = WireDefault(0.6931471805599453.F(fractionalWidth.BP))
  val bias1 = WireDefault(1.353.F(fractionalWidth.BP))
  val k1 = WireDefault(0.3585.F(fractionalWidth.BP))
  val bias2 = WireDefault(0.344.F(fractionalWidth.BP))

  z := io.x / log2.asSInt
  p := io.x.asFixedPoint(fractionalWidth.BP) + z.asFixedPoint(fractionalWidth.BP) * log2
  lp := k1 * (p + bias1) * (p + bias1) + bias2
  io.exp_x := (lp >> z.asUInt).asSInt
}
