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

class MultiPEFloat2Fxp(val numPE: Int = 16) extends Module with SoftmaxAccuracy with DebugLog {
val io = IO(new Bundle {
    val x = Input(Valid(Vec(numPE, UInt((width).W))))
    val fp_x = Output(Vec(numPE, UInt((I + F).W)))
    val valid = Output(Bool())
    val reset = Input(Bool())
  })
  val ROUND = 1
  val x_fpvalidDelayed = ShiftRegister(io.x.valid, I + F + 4)
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
    val reset = Input(Bool())
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
    val reset = Input(Bool())
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





//输入一行，输出一行
class Softmax(val arraySize: Int = 512,val numPE: Int = 16,queueDepth: Int = 100) extends Module with SoftmaxAccuracy with DebugLog {
  val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(arraySize, UInt(width.W))))
    val soft_x = Decoupled(Vec(arraySize, UInt(width.W)))
  })
  //状态机定义
  object state extends ChiselEnum {
    val idle, compute, update, done,delay = Value
  }
  io.x.ready:=true.B
  io.soft_x.valid := false.B
  io.soft_x.bits := DontCare
  //io.soft_x.ready:=true.B

    //queue  x->x_use
  val queue = Module(new Queue(Vec(arraySize, UInt(width.W)),queueDepth))
  val x_use_reg=Reg(Vec(arraySize, Valid(UInt(width.W))))
  queue.io.enq.bits:=io.x.bits
  queue.io.enq.valid:=io.x.valid
  when(queue.io.deq.valid && queue.io.deq.ready) {
  for (i <- 0 until arraySize) {
    x_use_reg(i).bits := queue.io.deq.bits(i) 
    x_use_reg(i).valid := queue.io.deq.valid 
  }
  }
    


  //Float2FxpModules  x_use->x_fp 待测试
  //printf("io.x.valid: %d,io.x.bits: %d\n",io.x.valid,io.x.bits(0))  
  //printf("queue.io.enq.valid: %d,queue.io.enq.bits: %d\n",queue.io.enq.valid,queue.io.enq.bits(0))
  //printf("queue.io.deq.valid: %d,queue.io.deq.bits: %d\n",queue.io.deq.valid,queue.io.deq.bits(0))
  //printf("x_use_reg.valid: %d,x_use_reg.bits: %d\n",x_use_reg.valid,x_use_reg.bits(0))
  //when(queue.io.deq.valid){
    //printf("queue.io.deq.bits: %d %d\n",queue.io.deq.bits(0),queue.io.deq.bits(1))
  //}
  val Float2FxpModule=Module(new MultiPEFloat2Fxp(numPE))
  val Float2FxpModule_state=RegInit(state.idle)
  val expModule_state=RegInit(state.idle)
    val divModule_state=RegInit(state.idle)
  queue.io.deq.ready:=(Float2FxpModule_state===state.idle)&&(~x_use_reg.map(_.valid).reduce(_ || _))
  val Float2FxpModule_row=Counter(arraySize/numPE)
  val x_fp_reg=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val Float2FxpModule_reset = RegInit(false.B) // 复位标志
  val Float2FxpModule_done = RegInit(false.B) // 完成标志
  //输入连接
  for(i<-0 until numPE){
    Float2FxpModule.io.x.bits(i):=x_use_reg(Float2FxpModule_row.value*numPE.U+i.U).bits
  }

  Float2FxpModule.io.x.valid:=(0 until numPE).map(i => x_use_reg(Float2FxpModule_row.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(Float2FxpModule_state) {
    is(state.idle) {
      when(x_use_reg.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          x_use_reg(Float2FxpModule_row.value*numPE.U+i.U).valid:=false.B
        }
        //printf("false 1\n")

        Float2FxpModule_state:=state.compute
        //printf("row: %d,x_use_reg.bits: %d %d\n",Float2FxpModule_row.value,x_use_reg.bits(0),x_use_reg.bits(1))
        //printf("compute\n")
      }
    }
    is(state.compute) {
      when(Float2FxpModule.io.valid) {
        for (i <- 0 until numPE) {
          x_fp_reg(Float2FxpModule_row.value*numPE.U+i.U).bits:=Float2FxpModule.io.fp_x(i)
        }
        //printf("row: %d,x_fp_reg.bits: %d %d\n",Float2FxpModule_row.value,x_fp_reg.bits(0),x_fp_reg.bits(1))
        Float2FxpModule_reset:= true.B
        Float2FxpModule_state:=state.update
        //printf("update\n")
      }
    }

    is(state.update) {
      
      when(Float2FxpModule_row.inc()) {
        Float2FxpModule_state:=state.done 
       // printf("done\n")
      }.otherwise {
        
        Float2FxpModule_state:=state.delay
       //printf("compute\n")
      }

    }
    is(state.delay) {
      //printf("Float2FxpModule_row: %d\n",Float2FxpModule_row.value.asUInt)
      when((0 until numPE).map(i => x_use_reg(Float2FxpModule_row.value*numPE.U+i.U).valid).reduce(_ && _)) {
          for(i<-0 until numPE){
            x_use_reg(Float2FxpModule_row.value*numPE.U+i.U).valid:=false.B
          }
          //printf("false 2\n")
        }
        Float2FxpModule_state:=state.compute
    }
    is(state.done) {
      Float2FxpModule_done:= true.B
      when(expModule_state===state.idle){
        for(i<-0 until arraySize){
          x_fp_reg(i).valid:=true.B
        }
      //printf("x_use_reg.valid: %d %d\n",x_use_reg(0).valid,x_use_reg(4).valid)
      Float2FxpModule_state:=state.idle
      //printf("idle x_fp_reg.bits: %d %d\n",x_fp_reg(0).bits,x_fp_reg(4).bits)
      }
      
    }
  }
  when(Float2FxpModule_reset) {
    Float2FxpModule.io.reset := true.B
    Float2FxpModule_reset := false.B
   // printf("reset\n")
  }.otherwise {
    Float2FxpModule.io.reset := false.B
  }
  //printf("x_fp_reg.valid: %d,x_fp_reg.bits: %d\n",x_fp_reg.valid,x_fp_reg.bits(0))
  //when(x_fp_reg.valid){
    //printf("x_fp_reg.bits: %d %d\n",x_fp_reg.bits(0),x_fp_reg.bits(4))
  //}

   //max
  val max = RegInit(0.U((I + F).W))
  max := x_fp_reg.reduceTree((a, b) => Mux(a.bits > b.bits, a, b)).bits



  
  
  
  //find all the exp(x - max)  x_fp->exp_x_fp
  val expModule=Module(new MultiPEFxpExp(numPE))

  val expModule_row=Counter(arraySize/numPE)
  val exp_x_fp_reg=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val expModule_reset = RegInit(false.B) // 复位标志
  val expModule_done = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    expModule.io.x.bits(i):=x_fp_reg(expModule_row.value*numPE.U+i.U).bits
  }
  expModule.io.max:=max
  expModule.io.x.valid:=(0 until numPE).map(i => x_fp_reg(expModule_row.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(expModule_state) {
    is(state.idle) {
      when(x_fp_reg.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          x_fp_reg(expModule_row.value*numPE.U+i.U).valid:=false.B
        }
        expModule_state:=state.compute
        //printf("exp compute\n")
      }
    }
    is(state.compute) {
      when(expModule.io.valid) {
        for (i <- 0 until numPE) {
          exp_x_fp_reg(expModule_row.value*numPE.U+i.U).bits:=expModule.io.exp_x(i)
        }
        expModule_reset:= true.B
        expModule_state:=state.update
       // printf("update\n")
      }
    }

    is(state.update) {
      when(expModule_row.inc()) {
        expModule_state:=state.done  
        //printf("done\n")
      }.otherwise {
        expModule_state:=state.delay
        //("compute\n") 
      }

    }
    is(state.delay) {
      when((0 until numPE).map(i => x_fp_reg(expModule_row.value*numPE.U+i.U).valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          x_fp_reg(expModule_row.value*numPE.U+i.U).valid:=false.B
        }
      }
      expModule_state:=state.compute
    }
    is(state.done) {
      expModule_done:= true.B
      when(divModule_state===state.idle){
        for(i<-0 until arraySize){
          exp_x_fp_reg(i).valid:=true.B
        }
        expModule_state:=state.idle
        //printf("idle\n")
      }
    }
  }
  when(expModule_reset) {
    expModule.io.reset := true.B
    expModule_reset := false.B
    //printf("reset\n")
  }.otherwise {
    expModule.io.reset := false.B
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
  //printf("Float2FxpModule_state: %d\n",Float2FxpModule_state.asUInt)
  //when(queue.io.deq.valid&&queue.io.deq.ready){
   // printf("queue.io.deq.bits: %d %d\n",queue.io.deq.bits(0),queue.io.deq.bits(4))
  //}

  //when((0 until arraySize).map(i => x_fp_reg(i).valid).reduce(_ && _)){
  //  printf("expModule_state: %d,x_fp_reg.bits: %d %d\n",expModule_state.asUInt,x_fp_reg(0).bits,x_fp_reg(4).bits)
 // }
  //when(exp_x_fp_reg.valid){
   // printf("exp_x_fp_reg.bits: %d %d\n",exp_x_fp_reg.bits(0),exp_x_fp_reg.bits(1))
 // }
  val exp_sum_fp=exp_x_fp_reg.map(_.bits).reduce((a, b) => a + b)
  val divModule=Module(new MultiPEFxpDiv(numPE))
  val divModule_row=Counter(arraySize/numPE)
  val soft_x_fp_reg=Reg((Vec(arraySize,  Valid(UInt((I + F).W)))))
  val divModule_reset = RegInit(false.B) // 复位标志
  val divModule_done = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    divModule.io.dividend(i).bits:=exp_x_fp_reg(divModule_row.value*numPE.U+i.U).bits
    divModule.io.dividend(i).valid:=exp_x_fp_reg(divModule_row.value*numPE.U+i.U).valid
  }
  divModule.io.divisor.bits:=exp_sum_fp
  divModule.io.divisor.valid:=(0 until numPE).map(i => exp_x_fp_reg(divModule_row.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(divModule_state) {
    is(state.idle) {
      when(exp_x_fp_reg.map(_.valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          exp_x_fp_reg(divModule_row.value*numPE.U+i.U).valid:=false.B
        }
        divModule_state:=state.compute
        //printf("compute\n")
      }
    }
    is(state.compute) {
      when(divModule.io.valid) {
        for (i <- 0 until numPE) {
          soft_x_fp_reg(divModule_row.value*numPE.U+i.U).bits:=divModule.io.out(i).bits
        }
        divModule_reset:= true.B
        divModule_state:=state.update
        //printf("update\n")
      }
    }

    is(state.update) {
      when(divModule_row.inc()) {
        divModule_state:=state.done  
        //printf("done\n")
      }.otherwise {
        divModule_state:=state.delay
        //printf("compute\n")
      }

    }
    is(state.delay) {
      when((0 until numPE).map(i => exp_x_fp_reg(divModule_row.value*numPE.U+i.U).valid).reduce(_ && _)) {
        for(i<-0 until numPE){
          exp_x_fp_reg(divModule_row.value*numPE.U+i.U).valid:=false.B
        }
      }
      divModule_state:=state.compute
    }
    is(state.done) {
      divModule_done:= true.B
      for(i<-0 until arraySize){
        soft_x_fp_reg(i).valid:=true.B
      }
      io.soft_x.valid := true.B
      io.soft_x.bits := soft_x_fp_reg.map(_.bits)
      divModule_state:=state.idle
      //printf("idle\n")
    }
  }
  when(divModule_reset) {
    divModule.io.reset := true.B
    divModule_reset := false.B
  }.otherwise {
    divModule.io.reset := false.B
  }
  //printf("divModule_state: %d\n",divModule_state.asUInt)
 // printf("exp_x_fp_reg.valid: %d,exp_x_fp_reg.bits: %d\n",exp_x_fp_reg.valid,exp_x_fp_reg.bits(0))
  //printf("soft_x_fp_reg.valid: %d,soft_x_fp_reg.bits: %d\n",soft_x_fp_reg.valid,soft_x_fp_reg.bits(0))
  //printf("io.soft_x.valid: %d,io.soft_x.bits: %d\n",io.soft_x.valid,io.soft_x.bits(0))
}