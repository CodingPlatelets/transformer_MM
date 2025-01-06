package kernel.alu

import chisel3._
import chisel3.util._
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog
import kernel.deprecated.PE

class currentRowIndex(
  val m: Int,
  val n: Int
)(
  implicit config: DataWidthConfig)
    extends Bundle {
  val index = Output(UInt(log2Ceil(m).W)) //输出的行索引
  val value = Output(Vec(n, UInt(config.outputWidth.W))) //输出的行值
}

// input: matrixA_row: one row of matrixA : 1 * k
// input: matrixB_cols: peCount rows of matrixB : k * peCount
// input: reset: clear pes old data
// output: blockResult: one block of result : 1 * peCount
class MultiFMA(
  val k:        Int,
  val peCount:  Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val matrixA_row = Flipped(Decoupled(Vec(k, UInt(config.inputWidth.W))))
    val matrixB_cols = Flipped(Decoupled(Vec(k, Vec(peCount, UInt(config.inputWidth.W)))))
    val blockResult = Decoupled(Vec(peCount, UInt(config.outputWidth.W)))
    val reset = Input(Bool())
  })

  val dataValid = io.matrixA_row.valid && io.matrixB_cols.valid

  val readyReg = RegInit(true.B)
  io.matrixA_row.ready := readyReg
  io.matrixB_cols.ready := readyReg
  io.blockResult.valid := false.B
  io.blockResult.bits := DontCare

  val pes = Seq.fill(peCount)(gemmType match {
    case GEMMDataType.Fxp  => Module(new PEFxp()).io
    case GEMMDataType.Fp32 => Module(new PEFp()).io
    case GEMMDataType.Fp64 => Module(new PEFp()).io
    case _                 => throw new IllegalArgumentException("Unsupported GEMM type")
  })

  val optIndex = RegInit(0.U(log2Ceil(k).W))
  val validReg = RegInit(false.B)

  for (i <- 0 until peCount) {
    pes(i).reset := io.reset
    pes(i).in_h := io.matrixA_row.bits(optIndex)
    pes(i).in_v := io.matrixB_cols.bits(optIndex)(i)
    io.blockResult.bits(i) := pes(i).out
  }
  io.blockResult.valid := validReg

  when(dataValid) {
    readyReg := false.B
  }
  when(io.reset) {
    optIndex := 0.U
    validReg := false.B
  }.elsewhen(optIndex === (k - 1).U) {
    optIndex := 0.U
    validReg := true.B
    readyReg := true.B
  }.otherwise {
    validReg := false.B
    optIndex := optIndex + 1.U
  }

  // TODO: FSM :reset logic is not correct, need to be fixed

  // pes.foreach { pe =>
  //   pe.in_h := 0.U
  //   pe.in_v := 0.U
  //   pe.reset := DontCare
  // }

  // io.blockResult.valid := validReg
  // io.blockResult.bits := DontCare

  // object state extends ChiselEnum {
  //   val idle, reset, compute, update, done = Value
  // }
  // val stateReg = RegInit(state.idle)

  // switch(stateReg) {
  //   is(state.idle) {
  //     when(dataValid) {
  //       readyReg := false.B
  //       stateReg := state.compute
  //     }
  //   }
  //   is(state.compute) {
  //     when(io.reset) {
  //       stateReg := state.reset
  //     }
  //     for (i <- 0 until peCount) {
  //       pes(i).reset := io.reset
  //       pes(i).in_h := io.matrixA_row.bits(optIndex)
  //       pes(i).in_v := io.matrixB_cols.bits(optIndex)(i)
  //       io.blockResult.bits(i) := pes(i).out
  //     }

  //     // printf(p"optIndex: ${optIndex}\n")
  //     // printf(p"io.matrixA_row.bits(${optIndex}): ${io.matrixA_row.bits(optIndex)}\n")
  //     // for (i <- 0 until peCount) {
  //     //   printf(p"pe: $i\n")
  //     //   printf(p"io.matrixB_cols.bits(${optIndex})($i): ${io.matrixB_cols.bits(optIndex)(i)}\n")
  //     //   printf(p"io.blockResult.bits(${i}): ${io.blockResult.bits(i)}\n")
  //     // }
  //     stateReg := state.update
  //   }
  //   is(state.reset) {
  //     optIndex := 0.U
  //     validReg := false.B
  //     stateReg := state.idle
  //   }
  //   is(state.update) {
  //     validReg := false.B
  //     when(optIndex === (k - 1).U) {
  //       stateReg := state.done
  //     }.otherwise {
  //       optIndex := optIndex + 1.U
  //       stateReg := state.compute
  //     }
  //   }
  //   is(state.done) {
  //     optIndex := 0.U
  //     readyReg := true.B
  //     validReg := true.B
  //     stateReg := state.idle
  //   }
  // }
}

// input: matrixA: m * k
// input: matrixB: k * n
// output: matrixC: m * n
class GEMMFMATotal(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  require(m % peCount == 0 && k % peCount == 0 && n % peCount == 0, "Matrix dimensions must be divisible by peCount")
  val io = IO(new Bundle {
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val results = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
  })

  val dataValid = io.matrixA.valid && io.matrixB.valid

  val readyReg = RegInit(true.B)
  io.matrixA.ready := readyReg
  io.matrixB.ready := readyReg
  io.results.valid := false.B
  io.results.bits := DontCare

  val multiFMA = Module(new MultiFMA(k, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.matrixA.valid
  multiFMA.io.matrixA_row.bits := io.matrixA.bits(rowIndex.value)

  multiFMA.io.matrixB_cols.valid := io.matrixB.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(k) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      io.matrixB.bits(j)((colIndex.value * peCount.U + i.U) % n.U)
    })
  }) //k * peCount size block of matrixB

  multiFMA.io.reset := true.B
  multiFMA.io.blockResult.ready := false.B

  val resultsReg = Reg(Vec(m, Vec(n, UInt(config.outputWidth.W))))

  object state extends ChiselEnum {
    val idle, compute, update, done = Value
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
      multiFMA.io.reset := false.B
      multiFMA.io.blockResult.ready := true.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          resultsReg(rowIndex.value)((colIndex.value * peCount.U + i.U) % n.U) := multiFMA.io.blockResult.bits(i)
        }
        stateReg := state.update
      }
    }

    is(state.update) {
      multiFMA.io.reset := true.B
      multiFMA.io.blockResult.ready := false.B
      when(colIndex.inc()) {
        when(rowIndex.inc()) {
          stateReg := state.done
        }.otherwise {
          stateReg := state.compute
        }
      }.otherwise {
        stateReg := state.compute
      }

    }
    is(state.done) {
      readyReg := true.B
      io.results.valid := true.B
      io.results.bits := resultsReg
      stateReg := state.idle
    }
  }

}

//input: matrixA: m * k
//input: matrixB: k * n
//output: currentRowIndex: one row of matrixC: 1 * n and current row index
//output: done: total matrixC finish flag
class GEMMFMASingle(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  require(m % peCount == 0 && k % peCount == 0 && n % peCount == 0, "Matrix dimensions must be divisible by peCount")
  val io = IO(new Bundle {
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val currentRow = Decoupled(new currentRowIndex(m, n))
    val done = Output(Bool())
  })

  val dataValid = io.matrixA.valid && io.matrixB.valid
  val readyReg = RegInit(true.B)
  io.matrixA.ready := readyReg
  io.matrixB.ready := readyReg
  io.currentRow.valid := false.B
  io.currentRow.bits := DontCare
  io.done := false.B

  val multiFMA = Module(new MultiFMA(k, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.matrixA.valid
  multiFMA.io.matrixA_row.bits := io.matrixA.bits(rowIndex.value)

  multiFMA.io.matrixB_cols.valid := io.matrixB.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(k) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      io.matrixB.bits(j)((colIndex.value * peCount.U + i.U) % n.U)
    })
  }) //k * peCount size block of matrixB

  multiFMA.io.reset := false.B
  multiFMA.io.blockResult.ready := true.B

  val currentRowReg = Reg(Vec(n, UInt(config.outputWidth.W)))

  object state extends ChiselEnum {
    val idle, compute, update, done = Value
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
      multiFMA.io.reset := false.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          currentRowReg((colIndex.value * peCount.U + i.U) % n.U) := multiFMA.io.blockResult.bits(i)
        }
        stateReg := state.update
      }
    }

    is(state.update) {
      multiFMA.io.reset := true.B
      io.currentRow.valid := false.B
      when(colIndex.inc()) {
        io.currentRow.valid := true.B
        io.currentRow.bits.index := rowIndex.value
        io.currentRow.bits.value := currentRowReg
        when(rowIndex.inc()) {
          stateReg := state.done
        }.otherwise {
          stateReg := state.compute
        }
      }.otherwise {
        stateReg := state.compute
      }

    }
    is(state.done) {
      io.done := true.B
      readyReg := true.B
      stateReg := state.idle
    }
  }
}

class GEMMSingleQueue(
  val m:          Int,
  val k:          Int,
  val n:          Int,
  val peCount:    Int = 16,
  val gemmType:   GEMMDataType.Type,
  val bufferSize: Int = 32
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W))))) // 矩阵A
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W))))) // 矩阵B
    val flush = Input(Bool())
    val currentRow = Decoupled(new currentRowIndex(m, n))
    val done = Output(Bool())
  })

  val currentBuffer = Module(
    new Queue(
      new currentRowIndex(m, n),
      entries = bufferSize,
      pipe = true,
      flow = false,
      useSyncReadMem = false,
      hasFlush = true
    )
  )
  val gemm = Module(new GEMMFMASingle(m, k, n, peCount, gemmType))
  gemm.io.matrixA <> io.matrixA
  gemm.io.matrixB <> io.matrixB
  currentBuffer.io.flush.get := io.flush
  currentBuffer.io.enq <> gemm.io.currentRow
  io.currentRow <> currentBuffer.io.deq
  io.done := gemm.io.done

}
