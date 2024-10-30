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
