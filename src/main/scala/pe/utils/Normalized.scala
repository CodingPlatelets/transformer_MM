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
  def fxpAdder(a: UInt, b: UInt): UInt = {
    val adder = Module(new FxpAdd(WII, WIF, WII, WIF, WII + log2Ceil(ArraySize), WIF))
    adder.io.ina := a
    adder.io.inb := b
    adder.io.out.bits
  }

  // 使用 Vec.reduceTree 进行并行累加
  val valuesTmp = RegInit(VecInit(Seq.fill(ArraySize)(0.U((WII + WIF).W))))
  val sumTree = valuesTmp.reduceTree(fxpAdder)

  // 除法器实例
  val divider = Module(new FxpDiv(WII + log2Ceil(ArraySize), WIF, log2Ceil(ArraySize) + 1, 0, WOI, WOF))
  divider.io.dividend := sumTree
  divider.io.divisor := ArraySize.U((log2Ceil(ArraySize) + 1).W)
  val dividerRes = RegInit(0.U((WOI + WOF).W))

  // 状态机
  val idle :: summing :: dividing :: Nil = Enum(3)
  val state = RegInit(idle)

  val dividerValid = RegInit(false.B)
  dividerValid := false.B
  io.out.valid := dividerValid

  // 计算所需的周期数
  val summingCycles = log2Ceil(ArraySize) * 2 // 每次加法需要2个周期
  val dividingCycles = WOI + WOF + 5 + 1 // 除法所需周期
  val totalCycles = summingCycles + dividingCycles
  val cycleCounter = RegInit(0.U(log2Ceil(totalCycles + 1).W))

  // 控制逻辑
  switch(state) {
    is(idle) {
      when(io.in.valid) {
        state := summing
        valuesTmp := io.in.bits
        cycleCounter := 0.U
      }
    }
    is(summing) {
      when(cycleCounter === (summingCycles - 1).U) {
        state := dividing
        cycleCounter := 0.U
      }.otherwise {
        cycleCounter := cycleCounter + 1.U
      }
    }
    is(dividing) {
      when(cycleCounter === (dividingCycles - 1).U) {
        state := idle
        dividerValid := true.B
        dividerRes := divider.io.out
      }.otherwise {
        cycleCounter := cycleCounter + 1.U
      }
    }
  }
  io.out.bits := dividerRes
  debugLog(
    p"state: $state, dividend: ${divider.io.dividend}, divisor: ${divider.io.divisor}, divider.io.out: ${divider.io.out}, dividerRes: $dividerRes, sumTree: ${sumTree}\n",
    LogLevel.DEBUG
  )
}
