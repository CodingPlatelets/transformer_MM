package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fixedpoint._
import kernel.utils.PipeValue
import kernel.utils.common

trait SoftmaxAccuracy {
  val I: Int = 8
  val F: Int = 24
  val width:Int=32
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

class SoftmaxPE(val arraySize: Int = 4) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(Vec(arraySize, UInt(width.W))))
    val soft_x = Output(Valid(Vec(arraySize, UInt(width.W))))
    val free = Output(Bool())
  })
 val freeReg = RegInit(true.B)
  when(io.x.valid) {
    freeReg := false.B // 当 x 有有效输入时，free 变为 false
  } .elsewhen(io.soft_x.valid) {
    freeReg := true.B // 当 soft_x 有有效输出时，free 变为 true
  }
  io.free := freeReg
  // first find the max value of x
  // cycle 1
  val ROUND = 1
  val Float2FxpModules = VecInit(Seq.fill(arraySize) {
    Module(new Float2FxpPipe(I, F, ROUND)).io
  })
  val x_fp = Wire(Valid(Vec(arraySize, UInt(width.W))))
  val x_fpvalidDelayed = ShiftRegister(io.x.valid, I + F + 4)
  for (col <- 0 until arraySize){
    Float2FxpModules(col).in := io.x.bits(col)
    x_fp.bits(col) := Float2FxpModules(col).out
  }
  x_fp.valid := x_fpvalidDelayed
 // x_fp.valid := RegNext(io.x.valid)
  val max = RegInit(0.U((I + F).W))
  max := x_fp.bits.reduceTree((a, b) => Mux(a > b, a, b))
  val xReg = RegNext(x_fp.bits)

  // cycle 2
  // then find all the exp(x - max)
  val expX = xReg.map { x =>
    val expALU = Module(new FixedPointExp)
    expALU.io.x.bits := (~(max - x) + 1.U).asSInt
    expALU.io.x.valid := RegNext(x_fp.valid)
    expALU.io.exp_x
  }

  val exp_sum = Pipe(expX.map(_.valid).reduce(_ & _), VecInit(expX.map(_.bits)).reduceTree(_ +& _), 0)

  // finally divide each exp(x - max) by exp_sum
  val softTmp = expX.map { exp =>
    val divModule = Module(new FxpDiv(I, F, I, F, I, F))
    val Fxp2FloatModule = Module(new Fxp2FloatPipe(I, F))
    val soft_x_xp = Wire(Valid(UInt(width.W)))
    divModule.io.dividend := exp
    divModule.io.divisor := exp_sum
    Fxp2FloatModule.io.in := divModule.io.out
    soft_x_xp := Fxp2FloatModule.io.out
    soft_x_xp
  }

  io.soft_x <> Pipe(softTmp.map(_.valid).reduce(_ & _), VecInit(softTmp.map(_.bits)), 0)
}





class Softmax(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val soft_x = Decoupled(Vec(arraySize, UInt(width.W)))
  })
    io.x.ready:=true.B
    val peElements = VecInit(Seq.fill(numPE){Module(new SoftmaxPE(arraySize)).io})
    val i=RegInit(0.U(log2Ceil(numPE).W))
    val j=RegInit(0.U(log2Ceil(numPE).W))
    val queue = Module(new Queue(Vec(arraySize, UInt(width.W)),queueDepth))
    val havefree=peElements.map(_.free).reduce(_|_)
    queue.io.enq.bits:=io.x.bits
    queue.io.enq.valid:=io.x.valid
    queue.io.deq.ready:=peElements(i).free
    for(k<-0 until numPE){
      peElements(k).x.bits:=Mux(queue.io.deq.valid && i===k.U,queue.io.deq.bits,VecInit(Seq.fill(arraySize)(0.U(width.W))))
      peElements(k).x.valid:=Mux(queue.io.deq.valid && i===k.U,queue.io.deq.valid,false.B)
    }
    //这里有问题
    for(k<-0 until numPE){
      io.soft_x.bits:=Mux(peElements(k).soft_x.valid && j===k.U,peElements(k).soft_x.bits,VecInit(Seq.fill(arraySize)(0.U(width.W))))
      io.soft_x.valid:=Mux(peElements(k).soft_x.valid && j===k.U,peElements(k).soft_x.valid,false.B)
    }
    //printf("i:%d j:%d havefree:%d 0free:%d\n",i,j,havefree,peElements(0).free)
    //printf("i:%d j:%d queue.io.deq.ready:%d queue.io.deq.valid:%d queue.io.deq.bits:%d\n",i,j,queue.io.deq.ready,queue.io.deq.valid,queue.io.deq.bits(0)  )
    //printf("queue.io.enq.valid:%d queue.io.enq.bits:%d\n",queue.io.enq.valid,queue.io.enq.bits(0))
    //printf("x.valid:%d x.bits:%d\n",io.x.valid,io.x.bits(0))
    //printf("peElements(0).x.bits:%d peElements(0).x.valid:%d\n",peElements(0).x.bits(0),peElements(0).x.valid)
    //printf("free:%d peElements(0).soft_x.bits:%d peElements(0).soft_x.valid:%d\n",peElements(0).free,peElements(0).soft_x.bits(0),peElements(0).soft_x.valid)
    printf("peElements(0).soft_x.valid:%d,peElements(0).soft_x.bits:%d\n",peElements(0).soft_x.valid,peElements(0).soft_x.bits(0))
    printf("io.soft_x.valid:%d,io.soft_x.bits:%d\n",io.soft_x.valid,io.soft_x.bits(0))
    i:=Mux(peElements(i).free|(~havefree),i,(i + 1.U) % numPE.U)
    j:=Mux(io.soft_x.valid,(j+1.U)%numPE.U,j)
}
