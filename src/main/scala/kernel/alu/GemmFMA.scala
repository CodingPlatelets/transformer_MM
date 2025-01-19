package kernel.alu

import chisel3._
import chisel3.util._
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog
import kernel.deprecated.PE

class curRowIndex(
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

}

class MultiFMA_v2(
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

  val optIndex = Counter(k)

  pes.foreach { pe =>
    pe.in_h := 0.U
    pe.in_v := 0.U
    pe.reset := DontCare
  }

  object state extends ChiselEnum {
    val idle, compute, update, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(io.reset) {
        optIndex.reset()
        for (i <- 0 until peCount) {
          pes(i).reset := true.B
        }
      }
      when(dataValid) {
        readyReg := false.B
        stateReg := state.compute
      }
    }
    is(state.compute) {
      for (i <- 0 until peCount) {
        pes(i).reset := false.B
        pes(i).in_h := io.matrixA_row.bits(optIndex.value)
        pes(i).in_v := io.matrixB_cols.bits(optIndex.value)(i)
        io.blockResult.bits(i) := pes(i).out
      }
      stateReg := state.update
    }
    is(state.update) {
      when(optIndex.inc()) {
        stateReg := state.done
      }.otherwise {
        stateReg := state.compute
      }
    }
    is(state.done) {
      readyReg := true.B
      io.blockResult.valid := true.B
      stateReg := state.idle
    }
  }
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

  //TODO:ERR use readyReg and resValid case deadlock
  // cases "Exception in thread "chiseltest_thread_2" java.lang.RuntimeException: Deadlock!" Error
  // when test fork() and join() in chiseltest


  //TODO:
  // all the moudle use readyReg can case deadlock
  // but use true.B ,false.B can't
  // when fork() and join() in chiseltest
  // maybe something wrong not find

  // val readyReg = RegInit(true.B)
  // val resValid = RegInit(false.B)
  // io.matrixA.ready := readyReg
  // io.matrixB.ready := readyReg
  // io.results.valid := resValid
  io.matrixA.ready := true.B
  io.matrixB.ready := true.B
  io.results.valid := false.B
  io.results.bits := DontCare

  val multiFMA = Module(new MultiFMA_v2(k, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.matrixA.valid
  multiFMA.io.matrixA_row.bits := io.matrixA.bits(rowIndex.value)

  multiFMA.io.matrixB_cols.valid := io.matrixB.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(k) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      io.matrixB.bits(j)((colIndex.value * peCount.U + i.U)(log2Ceil(n) - 1, 0))
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
        // readyReg := false.B
        io.matrixA.ready := false.B
        io.matrixB.ready := false.B
        stateReg := state.compute
      }
    }

    is(state.compute) {
      multiFMA.io.reset := false.B
      multiFMA.io.blockResult.ready := true.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          resultsReg(rowIndex.value)((colIndex.value * peCount.U + i.U)(log2Ceil(n) - 1, 0)) := multiFMA.io.blockResult
            .bits(i)
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
      // resValid := true.B
      // readyReg := true.B
      io.matrixA.ready := true.B
      io.matrixB.ready := true.B
      io.results.valid := true.B
      io.results.bits := resultsReg
      stateReg := state.idle
    }
  }

}

//input: matrixA: m * k
//input: matrixB: k * n
//output: curRowIndex: one row of matrixC: 1 * n and cur row index
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
    val curRow = Decoupled(new curRowIndex(m, n))
    val done = Output(Bool())
  })

  val dataValid = io.matrixA.valid && io.matrixB.valid
  // val readyReg = RegInit(true.B)
  // io.matrixA.ready := readyReg
  // io.matrixB.ready := readyReg
  io.matrixA.ready := true.B
  io.matrixB.ready := true.B
  io.curRow.valid := false.B
  io.curRow.bits := DontCare
  io.done := false.B

  val multiFMA = Module(new MultiFMA(k, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.matrixA.valid
  multiFMA.io.matrixA_row.bits := io.matrixA.bits(rowIndex.value)

  multiFMA.io.matrixB_cols.valid := io.matrixB.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(k) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      io.matrixB.bits(j)((colIndex.value * peCount.U + i.U)(log2Ceil(n) - 1, 0))
    })
  }) //k * peCount size block of matrixB

  multiFMA.io.reset := true.B
  multiFMA.io.blockResult.ready := false.B

  val curRowReg = Reg(Vec(n, UInt(config.outputWidth.W)))

  object state extends ChiselEnum {
    val idle, compute, update, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        // readyReg := false.B
        io.matrixA.ready := false.B
        io.matrixB.ready := false.B
        stateReg := state.compute
      }
    }

    is(state.compute) {
      multiFMA.io.reset := false.B
      multiFMA.io.blockResult.ready := true.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          curRowReg((colIndex.value * peCount.U + i.U)(log2Ceil(n) - 1, 0)) := multiFMA.io.blockResult.bits(i)
        }
        stateReg := state.update
      }
    }

    is(state.update) {
      multiFMA.io.reset := true.B
      multiFMA.io.blockResult.ready := false.B
      when(colIndex.inc()) {
        io.curRow.valid := true.B
        io.curRow.bits.index := rowIndex.value
        io.curRow.bits.value := curRowReg
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
      multiFMA.io.reset := true.B
      multiFMA.io.blockResult.ready := false.B
      io.matrixA.ready := true.B
      io.matrixB.ready := true.B
      io.done := true.B
      // readyReg := true.B
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
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val flush = Input(Bool())
    val curRow = Decoupled(new curRowIndex(m, n))
    val done = Output(Bool())
  })

  val curBuffer = Module(
    new Queue(
      new curRowIndex(m, n),
      entries = bufferSize,
      pipe = true,
      flow = false,
      useSyncReadMem = false,
      hasFlush = true
    )
  )
  val doneReg = RegInit(false.B)
  val gemm = Module(new GEMMFMASingle(m, k, n, peCount, gemmType))
  gemm.io.matrixA <> io.matrixA
  gemm.io.matrixB <> io.matrixB
  curBuffer.io.flush.get := io.flush
  curBuffer.io.enq <> gemm.io.curRow
  doneReg := gemm.io.done
  io.curRow <> curBuffer.io.deq
  io.done := doneReg

}
