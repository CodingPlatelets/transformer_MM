package pe.utils

import chisel3._
import chisel3.util._
import pe.utils.FxpAdd
import svsim.Simulation.Command.Log

// will calculate the average of the input array for fixed-point number
// and will cost 2*log2(ArraySize) + WOI + WOF + 5 cycles
class AverageModule(
  val WII:       Int = 8,
  val WIF:       Int = 8,
  val WOI:       Int = 8,
  val WOF:       Int = 8,
  val ArraySize: Int = 16)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val in = Input(Valid(Vec(ArraySize, UInt((WII + WIF).W))))
    val out = Valid(UInt((WOI + WOF).W))
  })

  // 加法器实例
  def fxpAdder(a: Valid[UInt], b: Valid[UInt]) = {
    val adder = Module(new FxpAdd(WII, WIF, WII, WIF, WII + log2Ceil(ArraySize), WIF))
    adder.io.ina <> a
    adder.io.inb <> b
    adder.io.out
  }

  // storage the input vec
  val nums = VecInit(io.in.bits.map(num => Pipe(io.in.valid, num, 0)))

  val sumTree = nums.reduceTree { (a, b) =>
    fxpAdder(a, b)
  }

  // 除法器实例
  val divider = Module(new FxpDiv(WII + log2Ceil(ArraySize), WIF, log2Ceil(ArraySize) + WII, 0, WOI, WOF))
  divider.io.divisor.valid := true.B
  divider.io.divisor.bits := ArraySize.U
  divider.io.dividend := sumTree

  io.out <> divider.io.out

}

// calculate the standard deviation of the input array
// **attention**: the input IO is a Decoupled, which means you should storage the input when using this module until the input is ready
class StandardDeviationModule(
  val WII:       Int = 8,
  val WIF:       Int = 8,
  val WOI:       Int = 8,
  val WOF:       Int = 8,
  val ArraySize: Int = 16)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(ArraySize, UInt((WII + WIF).W))))
    val out = Valid(UInt((WOI + WOF).W))
  })

  // do average
  val averageModule = Module(new AverageModule(WII, WIF, WII, WIF, ArraySize))
  averageModule.io.in <> Pipe(io.in.fire, io.in.bits, 0)
  val average = averageModule.io.out

  val subAverage = Pipe(average.valid, Mux(average.bits.head(1) === 0.U, ~average.bits + 1.U, average.bits), 0)

  // do substract the average
  // first, we should storage the input array to wait for the average
  val nums = RegInit(VecInit.fill(ArraySize)(0.U((WII + WIF).W)))
  nums := Mux(io.in.fire, io.in.bits, nums)
  val subNums = nums.map(num => {
    val subModule = Module(new FxpAdd(WII, WIF, WII, WIF, WII, WIF))
    subModule.io.ina <> Pipe(subAverage.valid, num, 0)
    subModule.io.inb <> subAverage
    subModule.io.out
  })

  // do square
  val squareNums = subNums.map(num => {
    val mulModule = Module(new FxpMul(WII, WIF, WII, WIF, 2 * WII, 2 * WIF))
    mulModule.io.ina <> num
    mulModule.io.inb <> num
    mulModule.io.out
  })

  // do average
  val averageSquare = Module(new AverageModule(2 * WII, 2 * WIF, 2 * WII, 2 * WIF, ArraySize))
  averageSquare.io.in <> Pipe(squareNums.map(_.valid).reduce(_ & _), VecInit(squareNums.map(_.bits)), 0)
  val averageSquareValue = averageSquare.io.out

  // do sqrt
  val sqrtModule = Module(new FxpSqrt(2 * WII, 2 * WIF, WOI, WOF))
  sqrtModule.io.in <> averageSquareValue
  io.out <> sqrtModule.io.out

  val sdReady = RegInit(true.B)
  io.in.ready := sdReady
  // when the io.in.valid is high, set the sdReady to false, and until the averageModule.io.out.valid is high, reset the sdReady to true
  val state = RegInit(0.U(2.W))
  switch(state) {
    is(0.U) {
      sdReady := true.B
      when(io.in.fire) {
        state := 1.U
        sdReady := false.B
      }
    }
    is(1.U) {
      when(subAverage.valid) {
        state := 0.U
        sdReady := true.B
      }
    }
  }
}
