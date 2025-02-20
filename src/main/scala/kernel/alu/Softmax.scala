package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fixedpoint._
import kernel.utils.PipeValue
import kernel.utils.common
import scala.math.pow

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

class MultiPEFloat2Fxp(val numPE: Int = 16) extends Module with SoftmaxAccuracy with DebugLog {
val io = IO(new Bundle {
    val x = Input(Valid(Vec(numPE, UInt((width).W))))
    val fp_x = Output(Vec(numPE, UInt((I + F).W)))
    val valid = Output(Bool())
    //val reset = Input(Bool())
  })
  val ROUND = 1
  val x_fpvalidDelayed = ShiftRegister(io.x.valid, I + F + 3)
  val float2FxpModules =VecInit(Seq.fill(numPE) {
    Module(new Float2FxpPipe(I, F, ROUND)).io
  })
  for(i<-0 until numPE){
    float2FxpModules(i).in:=io.x.bits(i)
    io.fp_x(i):=float2FxpModules(i).out
  }
  io.valid:=x_fpvalidDelayed

}
//16pe运行一次 tested
class MultiPEFxpExp(val numPE: Int = 16) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Input(Valid(Vec(numPE, UInt((I + F).W))))
    val max = Input(UInt((I + F).W))
    val exp_x = Output(Vec(numPE, UInt((I + F).W)))
    val valid = Output(Bool())
    //val reset = Input(Bool())
  })
   val expModules =VecInit(Seq.fill(numPE) {
    Module(new FixedPointExp).io
  })
  for(i<-0 until numPE){
    expModules(i).x.bits:=(~(io.max - io.x.bits(i)) + 1.U).asSInt
    expModules(i).x.valid:=io.x.valid
    io.exp_x(i):=expModules(i).exp_x.bits
  }
  io.valid:=expModules.map(_.exp_x.valid).reduce(_ && _)
}



//16个除法单元除一次
class MultiPEFxpDiv(val numPE: Int = 16) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val dividend = Input(Vec(numPE, Valid(UInt((I + F).W))))
    val divisor = Input(Valid(UInt((I + F).W)))
    val out = Output(Vec(numPE, Valid(UInt((I + F).W))))
    val valid = Output(Bool())
    //val reset = Input(Bool())
  })
  val divModules =VecInit(Seq.fill(numPE) {
    Module(new FxpDiv(I, F, I, F, I, F)).io
  })
  for(i<-0 until numPE){
    divModules(i).dividend:=io.dividend(i)
    divModules(i).divisor:=io.divisor
    io.out(i):=divModules(i).out
  }
  io.valid:=divModules.map(_.out.valid).reduce(_ && _)
}

//tested
class SoftmaxStg1(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val expX = Decoupled(Vec(arraySize, UInt(width.W)))
  })

  io.x.ready:=true.B
  io.expX.valid := false.B
  io.expX.bits := DontCare
  //io.soft_x.ready:=true.B

    //queue  x->x_use
  val queue = Module(new Queue(Vec(arraySize, UInt(width.W)),queueDepth))
  val xReg=Reg(Vec(arraySize, Valid(UInt(width.W))))
  val xRegUsing=RegInit(false.B)//考虑使用counter
  val xFpUsing=RegInit(false.B)
  //val expXUsing=RegInit(false.B)
  queue.io.enq.bits:=io.x.bits
  queue.io.enq.valid:=io.x.valid
  when(queue.io.deq.valid && queue.io.deq.ready) {
  for (i <- 0 until arraySize) {
    xReg(i).bits := queue.io.deq.bits(i) 
    xReg(i).valid := queue.io.deq.valid 
  }
  //printf("xReg.bits: %d %d\n\n",xReg(0).bits,xReg(4).bits)
  }
    


  //Float2FxpModules  x_use->x_fp 待测试


  //when(queue.io.deq.valid){
    //printf("queue.io.deq.bits: %d %d\n",queue.io.deq.bits(0),queue.io.deq.bits(1))
  //}
  val float2FxpModule=Module(new MultiPEFloat2Fxp(numPE))
  
  val float2FxpModuleinRow=Counter(arraySize/numPE)
  val float2FxpModuleoutRow=Counter(arraySize/numPE)
  val xFp=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  queue.io.deq.ready:=(~xReg.map(_.valid).reduce(_ || _))&&(xRegUsing===false.B)//maybe
  //printf("queue.io.deq.ready: %d,xRegUsing: %d\n",queue.io.deq.ready,xRegUsing)
  //输入连接
  for(i<-0 until numPE){
    float2FxpModule.io.x.bits(i):=xReg(float2FxpModuleinRow.value*numPE.U+i.U).bits
  }
  float2FxpModule.io.x.valid:=(0 until numPE).map(i => xReg(float2FxpModuleinRow.value*numPE.U+i.U).valid).reduce(_ && _)
  when((0 until numPE).map(i => xReg(float2FxpModuleinRow.value*numPE.U+i.U).valid).reduce(_ && _)&&xFpUsing===false.B){
    xRegUsing:=true.B
   //printf("in,inRow: %d\n",float2FxpModuleinRow.value)
   for(i<-0 until numPE){
    xReg(float2FxpModuleinRow.value*numPE.U+i.U).valid:=false.B
   }
   float2FxpModuleinRow.inc()
  }.otherwise{
    float2FxpModule.io.x.valid:=false.B
  }
 
  when(float2FxpModule.io.valid){
   //printf("out,outRow: %d\n",float2FxpModuleoutRow.value)
   //printf("float2FxpModule.io.fp_x(0).bits: %d,\n",float2FxpModule.io.fp_x(0))
    for(i<-0 until numPE){
      xFp(float2FxpModuleoutRow.value*numPE.U+i.U).bits:=float2FxpModule.io.fp_x(i)
    }
    //printf("xFp(0).bits: %d xFp(4).bits: %d\n\n",xFp(0).bits,xFp(4).bits)
    when(float2FxpModuleoutRow.inc()){
      for(i<-0 until arraySize){
        xFp(i).valid:=true.B
      }
      xRegUsing:=false.B
      //printf("2Fxp done\n")
    }
  }
   //printf("valid: %d, float2FxpModule.io.fp_x(0).bits: %d,\n",float2FxpModule.io.valid,float2FxpModule.io.fp_x(0))
  //printf("float2FxpModuleRow: %d\n in valid: %d\n out valid: %d\n",float2FxpModuleRow.value,float2FxpModule.io.x.valid,float2FxpModule.io.valid)
  //printf("float2FxpModule.io.x.valid: %d,float2FxpModule.io.x.bits: %d\n",float2FxpModule.io.x.valid,float2FxpModule.io.x.bits(0))
 



  val max = RegInit(0.U((I + F).W))
  max := xFp.reduceTree((a, b) => Mux(a.bits > b.bits, a, b)).bits
  //find all the exp(x - max)  x_fp->exp_x_fp
  val expModule=Module(new MultiPEFxpExp(numPE))

  val expModuleinRow=Counter(arraySize/numPE)
  val expModuleoutRow=Counter(arraySize/numPE)
  val expX=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  for(i<-0 until numPE){
    expModule.io.x.bits(i):=xFp(expModuleinRow.value*numPE.U+i.U).bits
  }
  expModule.io.max:=max
  val expModuleRowDelayed = RegNext(expModuleinRow.value)
val xValidDelayed = RegNext(
  ((0 until numPE).map(i => xFp(expModuleRowDelayed * numPE.U + i.U).valid).reduce(_ && _)) &&
  (expModuleRowDelayed === 0.U || expModuleRowDelayed > 0.U) // 合并条件
)

// 原代码修改为
expModule.io.x.valid := xValidDelayed
  expModule.io.x.valid:=((0 until numPE).map(i => xFp(expModuleinRow.value*numPE.U+i.U).valid).reduce(_ && _)&&expModuleinRow.value===0.U)||(expModuleinRow.value>0.U&&(0 until numPE).map(i => xFp(expModuleinRow.value*numPE.U+i.U).valid).reduce(_ && _))

  //状态转移
when(xValidDelayed===true.B){

   //printf("in,inRow: %d,max: %d\n",expModuleinRow.value,expModule.io.max)
  //  when(expModuleinRow.value===0.U){
  //   printf("max: %d %d %d,xFp.valid:%d\n",max,expModule.io.max,xFp.map(_.bits).reduce(_ > _),xFp.map(_.valid).reduce(_ && _))
  //  }
   xFpUsing:=true.B
   for(i<-0 until numPE){
    xFp(expModuleinRow.value*numPE.U+i.U).valid:=false.B
   }
   expModuleinRow.inc()
  }.otherwise{
    expModule.io.x.valid:=false.B
  }
 
  when(expModule.io.valid){
   //printf("out,outRow: %d\n",float2FxpModuleoutRow.value)
   //printf("float2FxpModule.io.fp_x(0).bits: %d,\n",float2FxpModule.io.fp_x(0))
    for(i<-0 until numPE){
      expX(expModuleoutRow.value*numPE.U+i.U).bits:=expModule.io.exp_x(i)
    }
    //printf("expX(0).bits: %d expX(4).bits: %d\n\n",expX(0).bits,expX(4).bits)
    when(expModuleoutRow.inc()){
      for(i<-0 until arraySize){
        expX(i).valid:=true.B
      }
      xFpUsing:=false.B
      //printf("2Fxp done\n")
    }
  }
  when(expX.map(_.valid).reduce(_ && _)){
    io.expX.valid:=true.B
    io.expX.bits:=expX.map(_.bits)
    for(i<-0 until arraySize){
      expX(i).valid:=false.B
    }
  }
  // when(io.x.valid){
  //   printf("io.x.valid: %d,io.x.bits: %d %d\n",io.x.valid,io.x.bits(0),io.x.bits(4))  
  // }
  // when(queue.io.enq.valid){
  //   printf("queue.io.enq.valid: %d,queue.io.enq.bits: %d %d\n",queue.io.enq.valid,queue.io.enq.bits(0),queue.io.enq.bits(4))
  // }
  // when(queue.io.deq.valid&&queue.io.deq.ready){
  //   printf("queue.io.deq.valid: %d,queue.io.deq.bits: %d %d\n",queue.io.deq.valid&&queue.io.deq.ready,queue.io.deq.bits(0),queue.io.deq.bits(4))
  // }
  // when(xReg.map(_.valid).reduce(_ && _)){
  //   printf("xReg.valid: %d,xReg.bits: %d %d\n",xReg.map(_.valid).reduce(_ && _),xReg(0).bits,xReg(4).bits)
  // }
  // when(xFp.map(_.valid).reduce(_ && _)){
  //   printf("xFp.valid: %d,xFp.bits: %d %d\n\n",xFp.map(_.valid).reduce(_ && _),xFp(0).bits,xFp(4).bits)
  // }
  // when(io.expX.valid){
  //   printf("expX.valid: %d,expX.bits: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d \n",io.expX.valid,io.expX.bits(0),io.expX.bits(1),io.expX.bits(2),io.expX.bits(3),io.expX.bits(4),io.expX.bits(5),io.expX.bits(6),io.expX.bits(7),io.expX.bits(8),io.expX.bits(9),io.expX.bits(10),io.expX.bits(11),io.expX.bits(12),io.expX.bits(13),io.expX.bits(14),io.expX.bits(15))
  // }
}
class SoftmaxStg2(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val expX = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val soft_x = Decoupled(Vec(arraySize, UInt(width.W)))
  })
  object state extends ChiselEnum {
    val idle, compute, update, done,delay = Value
  }
  io.expX.ready:=true.B
  io.soft_x.valid:=false.B
  io.soft_x.bits:=DontCare
  val queue = Module(new Queue(Vec(arraySize, UInt(width.W)),queueDepth))
  val expXReg=Reg(Vec(arraySize, Valid(UInt(width.W))))
  queue.io.enq.bits:=io.expX.bits
  queue.io.enq.valid:=io.expX.valid
  queue.io.deq.ready:=true.B
  when(queue.io.deq.valid&&queue.io.deq.ready){
    for(i<-0 until arraySize){
      expXReg(i).bits:=queue.io.deq.bits(i)
      expXReg(i).valid:=queue.io.deq.valid
    }
  }
  val expSumFp=expXReg.map(_.bits).reduce((a, b) => a + b)
  val divModule=Module(new MultiPEFxpDiv(numPE))
  val divModuleRow=Counter(arraySize/numPE)
  val divModuleState=RegInit(state.idle)
  queue.io.deq.ready:=(divModuleState===state.idle)&&(~expXReg.map(_.valid).reduce(_ || _))
  val softX=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val divModuleReset = RegInit(false.B) // 复位标志
  val divModuleDone = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    divModule.io.dividend(i).bits:=expXReg(divModuleRow.value*numPE.U+i.U).bits
    divModule.io.dividend(i).valid:=expXReg(divModuleRow.value*numPE.U+i.U).valid
  }
  divModule.io.divisor.bits:=expSumFp
  divModule.io.divisor.valid:=(0 until numPE).map(i => expXReg(divModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(divModuleState) {
    is(state.idle) {
      when(expXReg.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          expXReg(divModuleRow.value*numPE.U+i.U).valid:=false.B
        }
        divModuleState:=state.compute
        //printf("compute\n")
      }
    }
    is(state.compute) {
      when(divModule.io.valid) {
        for (i <- 0 until numPE) {
          softX(divModuleRow.value*numPE.U+i.U).bits:=divModule.io.out(i).bits
        }
        divModuleReset:= true.B
        divModuleState:=state.update
        //printf("update\n")
      }
    }

    is(state.update) {
      when(divModuleRow.inc()) {
        divModuleState:=state.done  
        //printf("done\n")
      }.otherwise {
        divModuleState:=state.delay
        //printf("compute\n")
      }

    }
    is(state.delay) {
      when((0 until numPE).map(i => expXReg(divModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          expXReg(divModuleRow.value*numPE.U+i.U).valid:=false.B
        }
      }
      divModuleState:=state.compute
    }
    is(state.done) {
      divModuleDone:= true.B
      for(i<-0 until arraySize){
        softX(i).valid:=true.B
      } 
      io.soft_x.valid := true.B
      io.soft_x.bits := softX.map(_.bits)
      divModuleState:=state.idle
      //printf("idle\n")
    }
  }
  when(divModuleReset) {
    //divModule.io.reset := true.B
    divModuleReset := false.B
  }.otherwise {
    //divModule.io.reset := false.B
  }
// when(io.expX.valid){
//   printf("io.expX.valid: %d,io.expX.bits: %d %d\n\n",io.expX.valid,io.expX.bits(0),io.expX.bits(4))
// }
// when(queue.io.enq.valid){
//   printf("queue.io.enq.valid: %d,queue.io.enq.bits: %d %d\n\n",queue.io.enq.valid,queue.io.enq.bits(0),queue.io.enq.bits(4))
// }
// when(queue.io.deq.valid&&queue.io.deq.ready){
//   printf("queue.io.deq.valid: %d,queue.io.deq.bits: %d %d\n\n",queue.io.deq.valid,queue.io.deq.bits(0),queue.io.deq.bits(4))
// }
// when(expXReg.map(_.valid).reduce(_ && _)){
//   printf("expXReg.valid: %d,expXReg.bits: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d \n\n",expXReg.map(_.valid).reduce(_ && _),expXReg(0).bits,expXReg(1).bits,expXReg(2).bits,expXReg(3).bits,expXReg(4).bits,expXReg(5).bits,expXReg(6).bits,expXReg(7).bits,expXReg(8).bits,expXReg(9).bits,expXReg(10).bits,expXReg(11).bits,expXReg(12).bits,expXReg(13).bits,expXReg(14).bits,expXReg(15).bits)
// }

// when(softX.map(_.valid).reduce(_ && _)){
//   printf("softX.valid: %d,softX.bits: %d %d\n\n",softX.map(_.valid).reduce(_ && _),softX(0).bits,softX(4).bits)
// }

}


//one state machine
class Softmax(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val soft_x = Decoupled(Vec(arraySize, UInt(width.W)))
  })
  val softmaxStg1 = Module(new SoftmaxStg1(arraySize, numPE, queueDepth))
  val softmaxStg2 = Module(new SoftmaxStg2(arraySize, numPE, queueDepth))

  softmaxStg1.io.x <> io.x
  softmaxStg1.io.expX <> softmaxStg2.io.expX
  softmaxStg2.io.soft_x <> io.soft_x
  // when(softmaxStg1.io.expX.valid){
  //   printf("softmaxStg1.io.valid: %d,softmaxStg1.io.bits: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n\n",softmaxStg1.io.expX.valid,softmaxStg1.io.expX.bits(0),softmaxStg1.io.expX.bits(1),softmaxStg1.io.expX.bits(2),softmaxStg1.io.expX.bits(3),softmaxStg1.io.expX.bits(4),softmaxStg1.io.expX.bits(5),softmaxStg1.io.expX.bits(6),softmaxStg1.io.expX.bits(7),softmaxStg1.io.expX.bits(8),softmaxStg1.io.expX.bits(9),softmaxStg1.io.expX.bits(10),softmaxStg1.io.expX.bits(11),softmaxStg1.io.expX.bits(12),softmaxStg1.io.expX.bits(13),softmaxStg1.io.expX.bits(14),softmaxStg1.io.expX.bits(15))
  // }
}


//all state machine
class Softmaxold(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val soft_x = Decoupled(Vec(arraySize, UInt(width.W)))
  })
  object state extends ChiselEnum {
    val idle, compute, update, done,delay = Value
  }
  io.x.ready:=true.B
  io.soft_x.valid := false.B
  io.soft_x.bits := DontCare
  //io.soft_x.ready:=true.B

    //queue  x->x_use
  val queue = Module(new Queue(Vec(arraySize, UInt(width.W)),queueDepth))
  val xReg=Reg(Vec(arraySize, Valid(UInt(width.W))))
  queue.io.enq.bits:=io.x.bits
  queue.io.enq.valid:=io.x.valid
  when(queue.io.deq.valid && queue.io.deq.ready) {
  for (i <- 0 until arraySize) {
    xReg(i).bits := queue.io.deq.bits(i) 
    xReg(i).valid := queue.io.deq.valid 
  }
  //printf("xReg.bits: %d %d\n\n",xReg(0).bits,xReg(4).bits)
  }
    


  //Float2FxpModules  x_use->x_fp 待测试
  //printf("io.x.valid: %d,io.x.bits: %d\n",io.x.valid,io.x.bits(0))  
  //printf("queue.io.enq.valid: %d,queue.io.enq.bits: %d\n",queue.io.enq.valid,queue.io.enq.bits(0))
  //printf("queue.io.deq.valid: %d,queue.io.deq.bits: %d\n",queue.io.deq.valid,queue.io.deq.bits(0))
  //printf("xReg.valid: %d,xReg.bits: %d\n",xReg.valid,xReg.bits(0))
  //when(queue.io.deq.valid){
    //printf("queue.io.deq.bits: %d %d\n",queue.io.deq.bits(0),queue.io.deq.bits(1))
  //}
  val float2FxpModule=Module(new MultiPEFloat2Fxp(numPE))
  val float2FxpModuleState=RegInit(state.idle)
  val expModuleState=RegInit(state.idle)
  val divModuleState=RegInit(state.idle)
  queue.io.deq.ready:=(float2FxpModuleState===state.idle)&&(~xReg.map(_.valid).reduce(_ || _))
  val float2FxpModuleRow=Counter(arraySize/numPE)
  val xFp=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val float2FxpModuleReset = RegInit(false.B) // 复位标志
  val float2FxpModuleDone = RegInit(false.B) // 完成标志
  //输入连接
  for(i<-0 until numPE){
    float2FxpModule.io.x.bits(i):=xReg(float2FxpModuleRow.value*numPE.U+i.U).bits
  }

  float2FxpModule.io.x.valid:=(0 until numPE).map(i => xReg(float2FxpModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(float2FxpModuleState) {
    is(state.idle) {
      when(xReg.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          xReg(float2FxpModuleRow.value*numPE.U+i.U).valid:=false.B
        }
        //printf("false 1\n")

        float2FxpModuleState:=state.compute
        //printf("row: %d,xReg.bits: %d %d\n",float2FxpModuleRow.value,xReg.bits(0),xReg.bits(1))
        //printf("compute\n")
      }
    }
    is(state.compute) {
      when(float2FxpModule.io.valid) {
        for (i <- 0 until numPE) {
          xFp(float2FxpModuleRow.value*numPE.U+i.U).bits:=float2FxpModule.io.fp_x(i)
        }
        //printf("row: %d,x_fp_reg.bits: %d %d\n",Float2FxpModule_row.value,x_fp_reg.bits(0),x_fp_reg.bits(1))
        float2FxpModuleReset:= true.B
        float2FxpModuleState:=state.update
        //printf("update\n")
      }
    }

    is(state.update) {
      
      when(float2FxpModuleRow.inc()) {
        float2FxpModuleState:=state.done 
       // printf("done\n")
      }.otherwise {
        
        float2FxpModuleState:=state.delay
       //printf("compute\n")
      }

    }
    is(state.delay) {
      //printf("Float2FxpModule_row: %d\n",Float2FxpModule_row.value.asUInt)
      when((0 until numPE).map(i => xReg(float2FxpModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)) {
          for(i<-0 until numPE){
            xReg(float2FxpModuleRow.value*numPE.U+i.U).valid:=false.B
          }
          //printf("false 2\n")
        }
        float2FxpModuleState:=state.compute
    }
    is(state.done) {
      float2FxpModuleDone:= true.B
      when(expModuleState===state.idle){
        for(i<-0 until arraySize){
          xFp(i).valid:=true.B
        }
      //printf("xReg.valid: %d %d\n",xReg(0).valid,xReg(4).valid)
      float2FxpModuleState:=state.idle
      //printf("idle x_fp_reg.bits: %d %d\n",x_fp_reg(0).bits,x_fp_reg(4).bits)
      }
      
    }
  }
  when(float2FxpModuleReset) {
    //float2FxpModule.io.reset := true.B
    float2FxpModuleReset := false.B
   // printf("reset\n")
  }.otherwise {
    //float2FxpModule.io.reset := false.B
  }
  //printf("x_fp_reg.valid: %d,x_fp_reg.bits: %d\n",x_fp_reg.valid,x_fp_reg.bits(0))
  //when(x_fp_reg.valid){
    //printf("x_fp_reg.bits: %d %d\n",x_fp_reg.bits(0),x_fp_reg.bits(4))
  //}

   //max
  val max = RegInit(0.U((I + F).W))
  max := xFp.reduceTree((a, b) => Mux(a.bits > b.bits, a, b)).bits



  
  
  
  //find all the exp(x - max)  x_fp->exp_x_fp
  val expModule=Module(new MultiPEFxpExp(numPE))

  val expModuleRow=Counter(arraySize/numPE)
  val expX=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val expModuleReset = RegInit(false.B) // 复位标志
  val expModuleDone = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    expModule.io.x.bits(i):=xFp(expModuleRow.value*numPE.U+i.U).bits
  }
  expModule.io.max:=max
  expModule.io.x.valid:=(0 until numPE).map(i => xFp(expModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(expModuleState) {
    is(state.idle) {
      when(xFp.map(_.valid).reduce(_ && _)) {
        //printf("xFp(0).bits: %d xFp(4).bits: %d max: %d\n\n",xFp(0).bits,xFp(4).bits,max)
        for(i<-0 until numPE){
          xFp(expModuleRow.value*numPE.U+i.U).valid:=false.B
        }
        expModuleState:=state.compute
        //printf("exp compute\n")
      }
    }
    is(state.compute) {
      when(expModule.io.valid) {
        for (i <- 0 until numPE) {
          expX(expModuleRow.value*numPE.U+i.U).bits:=expModule.io.exp_x(i)
        }
        when(expModule.io.x.valid){
          printf("max: %d,expModuleinRow: %d\n",expModule.io.max,expModuleRow.value)
        }
        expModuleReset:= true.B
        expModuleState:=state.update
       // printf("update\n")
      }
    }

    is(state.update) {
      when(expModuleRow.inc()) {
        expModuleState:=state.done  
        //printf("done\n")
      }.otherwise {
        expModuleState:=state.delay
        //("compute\n") 
      }

    }
    is(state.delay) {
      when((0 until numPE).map(i => xFp(expModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          xFp(expModuleRow.value*numPE.U+i.U).valid:=false.B
        }
      }
      expModuleState:=state.compute
    }
    is(state.done) {
      expModuleDone:= true.B
      when(divModuleState===state.idle){
        for(i<-0 until arraySize){
          expX(i).valid:=true.B
        }
        expModuleState:=state.idle
        //printf("idle\n")
      }
    }
  }
  when(expModuleReset) {
    //expModule.io.reset := true.B
    expModuleReset := false.B
    //printf("reset\n")
  }.otherwise {
    //expModule.io.reset := false.B
  }
//printf("x_fp_reg.valid: %d,x_fp_reg.bits: %d\n",x_fp_reg.valid,x_fp_reg.bits(0))
//printf("exp_x_fp_reg.valid: %d,exp_x_fp_reg.bits: %d\n",exp_x_fp_reg.valid,exp_x_fp_reg.bits(0))






  //// finally divide each exp(x - max) by exp_sum
   /* when(io.x.valid){
      printf("io.x.bits: %d %d\n",io.x.bits(0),io.x.bits(4))
    }
    when(queue.io.enq.valid){
      printf("queue.io.enq.bits: %d %d\n",queue.io.enq.bits(0),queue.io.enq.bits(4))
    }*/
  //printf("float2FxpModuleState: %d\n",float2FxpModuleState.asUInt)
  //when(queue.io.deq.valid&&queue.io.deq.ready){
   // printf("queue.io.deq.bits: %d %d\n",queue.io.deq.bits(0),queue.io.deq.bits(4))
  //}

  //when((0 until arraySize).map(i => x_fp_reg(i).valid).reduce(_ && _)){
  //  printf("expModule_state: %d,x_fp_reg.bits: %d %d\n",expModule_state.asUInt,x_fp_reg(0).bits,x_fp_reg(4).bits)
 // }
  //when(exp_x_fp_reg.valid){
   // printf("exp_x_fp_reg.bits: %d %d\n",exp_x_fp_reg.bits(0),exp_x_fp_reg.bits(1))
 // }
  val expSumFp=expX.map(_.bits).reduce((a, b) => a + b)
  val divModule=Module(new MultiPEFxpDiv(numPE))
  val divModuleRow=Counter(arraySize/numPE)
  val softX=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val divModuleReset = RegInit(false.B) // 复位标志
  val divModuleDone = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    divModule.io.dividend(i).bits:=expX(divModuleRow.value*numPE.U+i.U).bits
    divModule.io.dividend(i).valid:=expX(divModuleRow.value*numPE.U+i.U).valid
  }
  divModule.io.divisor.bits:=expSumFp
  divModule.io.divisor.valid:=(0 until numPE).map(i => expX(divModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(divModuleState) {
    is(state.idle) {
      when(expX.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          expX(divModuleRow.value*numPE.U+i.U).valid:=false.B
        }
        divModuleState:=state.compute
        //printf("compute\n")
      }
    }
    is(state.compute) {
      when(divModule.io.valid) {
        for (i <- 0 until numPE) {
          softX(divModuleRow.value*numPE.U+i.U).bits:=divModule.io.out(i).bits
        }
        divModuleReset:= true.B
        divModuleState:=state.update
        //printf("update\n")
      }
    }

    is(state.update) {
      when(divModuleRow.inc()) {
        divModuleState:=state.done  
        //printf("done\n")
      }.otherwise {
        divModuleState:=state.delay
        //printf("compute\n")
      }

    }
    is(state.delay) {
      when((0 until numPE).map(i => expX(divModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          expX(divModuleRow.value*numPE.U+i.U).valid:=false.B
        }
      }
      divModuleState:=state.compute
    }
    is(state.done) {
      divModuleDone:= true.B
      for(i<-0 until arraySize){
        softX(i).valid:=true.B
      }
      io.soft_x.valid := true.B
      io.soft_x.bits := softX.map(_.bits)
      divModuleState:=state.idle
      //printf("idle\n")
    }
  }
  when(divModuleReset) {
    //divModule.io.reset := true.B
    divModuleReset := false.B
  }.otherwise {
    //divModule.io.reset := false.B
  }
  //  when(io.x.valid){
  //   printf("io.x.valid: %d,io.x.bits: %d %d\n",io.x.valid,io.x.bits(0),io.x.bits(4))  
  // }
  // when(queue.io.enq.valid){
  //   printf("queue.io.enq.valid: %d,queue.io.enq.bits: %d %d\n",queue.io.enq.valid,queue.io.enq.bits(0),queue.io.enq.bits(4))
  // }
  // when(queue.io.deq.valid&&queue.io.deq.ready){
  //   printf("queue.io.deq.valid: %d,queue.io.deq.bits: %d %d\n",queue.io.deq.valid&&queue.io.deq.ready,queue.io.deq.bits(0),queue.io.deq.bits(4))
  // }
  // when(xReg.map(_.valid).reduce(_ && _)){
  //   printf("xReg.valid: %d,xReg.bits: %d %d\n",xReg.map(_.valid).reduce(_ && _),xReg(0).bits,xReg(4).bits)
  // }
  // when(xFp.map(_.valid).reduce(_ && _)){
  //   printf("xFp.valid: %d,xFp.bits: %d %d\n\n",xFp.map(_.valid).reduce(_ && _),xFp(0).bits,xFp(4).bits)
  // // }
  // when(expX.map(_.valid).reduce(_ && _)){
  //   printf("expX.valid: %d,expX.bits: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d \n",expX.map(_.valid).reduce(_ && _),expX(0).bits,expX(1).bits,expX(2).bits,expX(3).bits,expX(4).bits,expX(5).bits,expX(6).bits,expX(7).bits,expX(8).bits,expX(9).bits,expX(10).bits,expX(11).bits,expX(12).bits,expX(13).bits,expX(14).bits,expX(15).bits)
  // }
  // when(io.soft_x.valid){
  //   printf("io.soft_x.valid: %d,io.soft_x.bits: %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d \n\n",io.soft_x.valid,io.soft_x.bits(0),io.soft_x.bits(1),io.soft_x.bits(2),io.soft_x.bits(3),io.soft_x.bits(4),io.soft_x.bits(5),io.soft_x.bits(6),io.soft_x.bits(7),io.soft_x.bits(8),io.soft_x.bits(9),io.soft_x.bits(10),io.soft_x.bits(11),io.soft_x.bits(12),io.soft_x.bits(13),io.soft_x.bits(14),io.soft_x.bits(15))
  // }
}


