package kernel.alu

import chisel3._
import chisel3.util._
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog
import kernel.deprecated.PE

// OutValue: get the final output value
// input: AttnWeights: m * m
// input: Value: m * n
// output: AttnOut: m * n
class OutValue(
  val m:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val Scores = Flipped(Decoupled(Vec(m, Vec(m, UInt(config.inputWidth.W)))))
    val Value = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val AttnOut = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
  })

  val dataValid = io.Scores.valid && io.Value.valid

  val readyReg = RegInit(true.B)
  io.Scores.ready := readyReg
  io.Value.ready := readyReg
  io.AttnOut.valid := false.B
  io.AttnOut.bits := DontCare

  val ValueMul = Module(new GEMMFMATotal(m, m, n, peCount, gemmType))

  ValueMul.io.matrixA.valid := io.Scores.valid
  ValueMul.io.matrixA.bits := io.Scores.bits
  ValueMul.io.matrixB.valid := io.Value.valid
  ValueMul.io.matrixB.bits := io.Value.bits
  ValueMul.io.results.ready := false.B

  object state extends ChiselEnum {
    val idle, compute, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.compute
      }
    }
    is(state.compute) {
      ValueMul.io.results.ready := true.B
      when(ValueMul.io.results.valid) {
        stateReg := state.done
      }
    }
    is(state.done) {
      ValueMul.io.results.ready := false.B
      readyReg := true.B
      io.AttnOut.valid := true.B
      io.AttnOut.bits := ValueMul.io.results.bits
      stateReg := state.idle
    }
  }
}

// OutValue: get the final output value
// input: one row of AttnWeights: 1 * m ,total m rows
// input: Value: m * n
// output: one row of AttnOut: 1 * n ,total m rows
// output: done: Bool
class OutValueSingle(
  val m:        Int,
  val n:        Int,
  val peCount:  Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val curScores = Flipped(Decoupled(new curRowIndex(m, m)))
    val Value = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val curAttnOut = Decoupled(new curRowIndex(m, n))
    val done = Output(Bool())
  })

  val dataValid = io.curScores.valid && io.Value.valid

  io.curScores.ready := true.B
  io.Value.ready := true.B
  io.curAttnOut.valid := false.B
  io.curAttnOut.bits := DontCare
  io.done := false.B

  val ValueReg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))
  ValueReg := io.Value.bits

  val multiFMA = Module(new MultiFMA(m, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.curScores.valid
  multiFMA.io.matrixA_row.bits := io.curScores.bits.value

  multiFMA.io.matrixB_cols.valid := io.Value.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(m) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      ValueReg(j)(((colIndex.value << log2Ceil(peCount).U) + i.U)(log2Ceil(n)-1, 0))
    })
  }) //m * peCount size block of Value

  multiFMA.io.reset := true.B
  multiFMA.io.blockResult.ready := false.B

  val curRowReg = Reg(Vec(n, UInt(config.outputWidth.W)))

  object state extends ChiselEnum {
    val idle, compute, update, load, done = Value
  }

  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        io.Value.ready := false.B
        stateReg := state.compute
      }
    }
    is(state.compute) {
      io.curScores.ready := false.B
      multiFMA.io.reset := false.B
      multiFMA.io.blockResult.ready := true.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          curRowReg(colIndex.value * peCount.U + i.U) := multiFMA.io.blockResult.bits(i)
        }
        stateReg := state.update
      }
    }
    is(state.update) {
      multiFMA.io.reset := true.B
      multiFMA.io.blockResult.ready := false.B
      io.curAttnOut.valid := false.B
      when(colIndex.inc()) {
        io.curAttnOut.valid := true.B
        io.curAttnOut.bits.index := rowIndex.value
        io.curAttnOut.bits.value := curRowReg
        when(rowIndex.inc()) {
          stateReg := state.done
        }.otherwise {
          stateReg := state.load
        }
      }.otherwise {
        stateReg := state.compute
      }
    }
    is(state.load) {
      io.curScores.ready := true.B
      stateReg := state.compute
    }
    is(state.done) {
      io.done := true.B
      io.Value.ready := true.B
      io.curScores.ready := true.B
      stateReg := state.idle
    }
  }
}
