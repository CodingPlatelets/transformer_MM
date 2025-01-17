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
    float2FxpModule.io.reset := true.B
    float2FxpModuleReset := false.B
   // printf("reset\n")
  }.otherwise {
    float2FxpModule.io.reset := false.B
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
    expModule.io.reset := true.B
    expModuleReset := false.B
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
  val divModule_reset = RegInit(false.B) // 复位标志
  val divModule_done = RegInit(false.B) // 完成标志
  for(i<-0 until numPE){
    divModule.io.dividend(i).bits:=expX(divModuleRow.value*numPE.U+i.U).bits
    divModule.io.dividend(i).valid:=expX(divModuleRow.value*numPE.U+i.U).valid
  }
  divModule.io.divisor.bits:=expSumFp
  divModule.io.divisor.valid:=(0 until numPE).map(i => expX(divModuleRow.value*numPE.U+i.U).valid).reduce(_ && _)
  //状态转移
  switch(divModule_state) {
    is(state.idle) {
      when(exp_x_fp_reg.map(_.valid).reduce(_ && _)) {
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
      divModule_done:= true.B
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
    divModule.io.reset := true.B
    divModuleReset := false.B
  }.otherwise {
    divModule.io.reset := false.B
  }
  //printf("divModule_state: %d\n",divModule_state.asUInt)
 // printf("exp_x_fp_reg.valid: %d,exp_x_fp_reg.bits: %d\n",exp_x_fp_reg.valid,exp_x_fp_reg.bits(0))
  //printf("soft_x_fp_reg.valid: %d,soft_x_fp_reg.bits: %d\n",soft_x_fp_reg.valid,soft_x_fp_reg.bits(0))
  //printf("io.soft_x.valid: %d,io.soft_x.bits: %d\n",io.soft_x.valid,io.soft_x.bits(0))
}


class ExpUnitFixPoint(width: Int, point: Int, lut_bits: Int, append_bits: Int) extends Module {
  val v_width = width + append_bits
  val v_point = point + append_bits
  val fpType = FixedPoint(width.W, point.BP)
  val vType = FixedPoint(v_width.W, v_point.BP)
  val io = IO(new Bundle {
    val in_value = Input(SInt(width.W))
    val out_exp = Output(UInt(width.W))
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
 
  io.out_exp := z2.asUInt
}