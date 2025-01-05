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

  multiFMA.io.reset := false.B
  multiFMA.io.blockResult.ready := true.B

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
      // printf(p"rowIndex: ${rowIndex.value}, colIndex: ${colIndex.value}\n")
      // printf(p"matrixA_row: ${multiFMA.io.matrixA_row.bits}\n")
      // for (i <- 0 until peCount) {
      //   printf(p"matrixB_cols(${i}): ${multiFMA.io.matrixB_cols.bits(i)}\n")
      // }
      multiFMA.io.reset := false.B
      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          resultsReg(rowIndex.value)((colIndex.value * peCount.U + i.U) % n.U) := multiFMA.io.blockResult.bits(i)
        }
        // for (i <- 0 until m) {
        //   for (j <- 0 until n) {
        //     printf(p"i:${i},j:${j},${resultsReg(i)(j)}\t")
        //   }
        //   printf(p"\n")
        // }
        stateReg := state.update
      }
    }

    is(state.update) {
      multiFMA.io.reset := true.B
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

// first use GEMMFMATotal to get Q and K, then use GEMMFMASingle to get Q*K^T
// out one row of score matrix
class AttnScoresSingle(
  val m:              Int,
  val k:              Int,
  val n:              Int,
  val peCount1:       Int = 16,
  val peCount2:       Int = 16,
  val gemmType:       GEMMDataType.Type,
  val bufferSizeGemm: Int = 32
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val scoreRow = Decoupled(new currentRowIndex(m, m))
    val resetBuffer = Input(Bool())
    val done = Output(Bool())
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.scoreRow.valid := false.B
  io.scoreRow.bits := DontCare
  io.done := false.B

  //use GEMMFMATotal to get Q and K
  val qGen = Module(new GEMMFMATotal(m, k, n, peCount1, gemmType))
  val kGen = Module(new GEMMFMATotal(m, k, n, peCount1, gemmType))
  qGen.io.matrixA <> io.inputToken
  qGen.io.matrixB <> io.weightQ
  kGen.io.matrixA <> io.inputToken
  kGen.io.matrixB <> io.weightK

  // when qGen and kGen are done, use GEMMFMASingle to get Q*K^T
  // Q: m * n, K: m * n -> Q*K^T: m * m
  val QK_TMul = Module(new GEMMSingleQueue(m, n, m, peCount2, gemmType, bufferSizeGemm))
  QK_TMul.io.matrixA <> qGen.io.results

  val K_T = VecInit(Seq.fill(n)(VecInit(Seq.fill(m)(0.U(config.inputWidth.W)))))
  for (i <- 0 until k) {
    for (j <- 0 until n) {
      K_T(i)(j) := kGen.io.results.bits(j)(i)
    }
  }

  QK_TMul.io.matrixB.valid := kGen.io.results.valid
  // QK_TMul.io.matrixB.bits := VecInit(kGen.io.results.bits.transpose.map(VecInit(_)))
  QK_TMul.io.matrixB.bits := K_T
  kGen.io.results.ready := QK_TMul.io.matrixB.ready

  QK_TMul.io.flush := io.resetBuffer
  io.scoreRow <> QK_TMul.io.currentRow

  object state extends ChiselEnum {
    val idle, gen, mul, collect, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.gen
      }
    }
    is(state.gen) {
      when(qGen.io.results.valid && kGen.io.results.valid) {
        debugLog(p"qGen results: ${qGen.io.results.bits}\n")
        debugLog(p"kGen results: ${kGen.io.results.bits}\n")
        stateReg := state.mul
      }
    }
    is(state.mul) {
      when(QK_TMul.io.currentRow.valid) {
        stateReg := state.collect
      }
    }
    is(state.collect) {
      when(QK_TMul.io.done) {
        stateReg := state.done
      }
    }
    is(state.done) {
      io.done := true.B
      readyReg := true.B
      stateReg := state.idle
    }
  }

}

class AttnScoresTotal(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val scores = Decoupled(Vec(m, Vec(m, UInt(config.outputWidth.W))))
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.scores.valid := false.B
  io.scores.bits := DontCare

  //use GEMMFMATotal to get Q and K
  val qGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))
  val kGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))
  qGen.io.matrixA := DontCare
  qGen.io.matrixB := DontCare
  kGen.io.matrixA := DontCare
  kGen.io.matrixB := DontCare
  qGen.io.results.ready := false.B
  kGen.io.results.ready := false.B

  // val Qreg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))
  // val Kreg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))

  // when qGen and kGen are done, use GEMMFMASingle to get Q*K^T
  // Q: m * n, K: m * n -> Q*K^T: m * m
  val QK_TMul = Module(new GEMMFMATotal(m, n, m, peCount, gemmType))
  QK_TMul.io.matrixA := DontCare
  QK_TMul.io.matrixB := DontCare
  QK_TMul.io.results.ready := false.B

  // object state extends ChiselEnum {
  //   val idle, gen, mul, done = Value
  // }
  // val stateReg = RegInit(state.idle)

  // switch(stateReg) {
  //   is(state.idle) {
  //     when(dataValid) {
  //       readyReg := false.B
  //       stateReg := state.gen
  //     }
  //   }
  //   is(state.gen) {
  //     when(qGen.io.results.valid && kGen.io.results.valid) {
  //       // Qreg := qGen.io.results.bits
  //       // Kreg := kGen.io.results.bits
  //       stateReg := state.mul
  //     }
  //   }
  //   is(state.mul) {
  //     when(QK_TMul.io.results.valid) {
  //       stateReg := state.done
  //     }
  //   }
  //   is(state.done) {
  //     readyReg := true.B
  //     stateReg := state.idle
  //   }
  // }

  when(dataValid) {
    readyReg := false.B
    qGen.io.matrixA <> io.inputToken
    qGen.io.matrixB <> io.weightQ
    kGen.io.matrixA <> io.inputToken
    kGen.io.matrixB <> io.weightK
    qGen.io.results.ready := true.B
    kGen.io.results.ready := true.B
    when(qGen.io.results.valid && kGen.io.results.valid) {
      QK_TMul.io.matrixA.valid := qGen.io.results.valid
      QK_TMul.io.matrixA.bits := qGen.io.results.bits
      qGen.io.results.ready := true.B
      QK_TMul.io.matrixB.valid := kGen.io.results.valid
      QK_TMul.io.matrixB.bits := VecInit(kGen.io.results.bits.transpose.map(VecInit(_)))
      kGen.io.results.ready := true.B
      when(QK_TMul.io.results.valid) {
        io.scores.valid := QK_TMul.io.results.valid
        io.scores.bits := QK_TMul.io.results.bits
        readyReg := true.B
      }
    }
  }
}

// QKGenWithReg: use two GEMMFMATotal to get Q and K
// whthin use Reg to store Q and K
// input: inputToken: m * k
// input: weightQ: k * n
// input: weightK: k * n
// output: Query: m * n
// output: Key: m * n
class QKGenWithReg(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val Query = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
    val Key = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.Key.valid := false.B
  io.Key.bits := DontCare
  io.Query.valid := false.B
  io.Query.bits := DontCare

  val qGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))
  val kGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))

  val Qreg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))
  val Kreg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))

  qGen.io.matrixA.bits := io.inputToken.bits
  qGen.io.matrixA.valid := io.inputToken.valid
  qGen.io.matrixB.bits := io.weightQ.bits
  qGen.io.matrixB.valid := io.weightQ.valid
  qGen.io.results.ready := true.B

  kGen.io.matrixA.bits := io.inputToken.bits
  kGen.io.matrixA.valid := io.inputToken.valid
  kGen.io.matrixB.bits := io.weightK.bits
  kGen.io.matrixB.valid := io.weightK.valid
  kGen.io.results.ready := true.B

  io.Query.valid := false.B
  io.Query.bits := Qreg
  io.Key.valid := false.B
  io.Key.bits := Kreg

  // qGen.io.matrixA <> io.inputToken
  // qGen.io.matrixB <> io.weightQ
  // kGen.io.matrixA <> io.inputToken
  // kGen.io.matrixB <> io.weightK

  // io.Query <> qGen.io.results
  // io.Key <> kGen.io.results

  object state extends ChiselEnum {
    val idle, gen, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.gen
      }
    }
    is(state.gen) {
      when(qGen.io.results.valid && kGen.io.results.valid) {
        Qreg := qGen.io.results.bits
        Kreg := kGen.io.results.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      readyReg := true.B
      io.Query.valid := true.B
      io.Key.valid := true.B
      stateReg := state.idle
    }
  }
}

// QKGen: use two GEMMFMATotal to get Q and K
// input: inputToken: m * k
// input: weightQ: k * n
// input: weightK: k * n
// output: Query: m * n
// output: Key: m * n
class QKGen(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val Query = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
    val Key = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.Key.valid := false.B
  io.Key.bits := DontCare
  io.Query.valid := false.B
  io.Query.bits := DontCare

  val qGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))
  val kGen = Module(new GEMMFMATotal(m, k, n, peCount, gemmType))

  // qGen.io.matrixA.bits := io.inputToken.bits
  // qGen.io.matrixA.valid := io.inputToken.valid
  // qGen.io.matrixB.bits := io.weightQ.bits
  // qGen.io.matrixB.valid := io.weightQ.valid
  // qGen.io.results.ready := true.B

  // kGen.io.matrixA.bits := io.inputToken.bits
  // kGen.io.matrixA.valid := io.inputToken.valid
  // kGen.io.matrixB.bits := io.weightK.bits
  // kGen.io.matrixB.valid := io.weightK.valid
  // kGen.io.results.ready := true.B

  // io.Query.valid := qGen.io.results.valid
  // io.Query.bits := qGen.io.results.bits
  // io.Key.valid := kGen.io.results.valid
  // io.Key.bits := kGen.io.results.bits

  qGen.io.matrixA <> io.inputToken
  qGen.io.matrixB <> io.weightQ
  kGen.io.matrixA <> io.inputToken
  kGen.io.matrixB <> io.weightK

  io.Query <> qGen.io.results
  io.Key <> kGen.io.results

  object state extends ChiselEnum {
    val idle, gen, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.gen
      }
    }
    is(state.gen) {
      when(qGen.io.results.valid && kGen.io.results.valid) {
        stateReg := state.done
      }
    }
    is(state.done) {
      readyReg := true.B
      io.Query.valid := true.B
      io.Key.valid := true.B
      stateReg := state.idle
    }
  }
}

// QKMulTotalWithReg: use GEMMFMATotal to get scores
// input: Query: m * n
// input: Key: m * n
// output: scores: m * m
class QKMulTotalWithReg(
  val m:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val Query = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val Key = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val scores = Decoupled(Vec(m, Vec(m, UInt(config.outputWidth.W))))
  })

  val dataValid = io.Query.valid && io.Key.valid

  val readyReg = RegInit(true.B)
  io.Query.ready := readyReg
  io.Key.ready := readyReg
  io.scores.valid := false.B
  io.scores.bits := DontCare

  val QK_TMul = Module(new GEMMFMATotal(m, n, m, peCount, gemmType))
  val scoresReg = Reg(Vec(m, Vec(m, UInt(config.outputWidth.W))))

  QK_TMul.io.matrixA.valid := io.Query.valid
  QK_TMul.io.matrixA.bits := io.Query.bits
  QK_TMul.io.matrixB.valid := io.Key.valid
  QK_TMul.io.matrixB.bits := VecInit(io.Key.bits.transpose.map(VecInit(_)))
  QK_TMul.io.results.ready := true.B

  // io.scores.valid := QK_TMul.io.results.valid
  // io.scores.bits := QK_TMul.io.results.bits
  io.scores.valid := false.B
  io.scores.bits := scoresReg

  object state extends ChiselEnum {
    val idle, mul, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.mul
      }
    }
    is(state.mul) {
      when(QK_TMul.io.results.valid) {
        // for (i <- 0 until m) {
        //   for (j <- 0 until n) {
        //     // printf(p"QK_TMul.io.matrixA.bits($i)($j): ${QK_TMul.io.matrixA.bits(i)(j)}\n")
        //     // printf(p"QK_TMul.io.matrixB.bits($i)($j): ${QK_TMul.io.matrixB.bits(i)(j)}\n")
        //     printf(p"io.Query.bits($i)($j): ${io.Query.bits(i)(j)}\n")
        //     printf(p"io.Key.bits($i)($j): ${io.Key.bits(i)(j)}\n")
        //   }
        // }
        // for (i <- 0 until m) {
        //   for (j <- 0 until m) {
        //     printf(p"QK_TMul.io.results.bits($i)($j): ${QK_TMul.io.results.bits(i)(j)}\n")
        //   }
        // }
        scoresReg := QK_TMul.io.results.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      readyReg := true.B
      io.scores.valid := true.B
      stateReg := state.idle
    }
  }
}

// QKMulTotal: use GEMMFMATotal to get scores
// input: Query: m * n
// input: Key: m * n
// output: scores: m * m
class QKMulTotal(
  val m:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val Query = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val Key = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val scores = Decoupled(Vec(m, Vec(m, UInt(config.outputWidth.W))))
  })

  val dataValid = io.Query.valid && io.Key.valid

  val readyReg = RegInit(true.B)
  io.Query.ready := readyReg
  io.Key.ready := readyReg
  io.scores.valid := false.B
  io.scores.bits := DontCare

  val QK_TMul = Module(new GEMMFMATotal(m, n, m, peCount, gemmType))

  QK_TMul.io.matrixA.valid := io.Query.valid
  QK_TMul.io.matrixA.bits := io.Query.bits
  QK_TMul.io.matrixB.valid := io.Key.valid
  QK_TMul.io.matrixB.bits := VecInit(io.Key.bits.transpose.map(VecInit(_)))
  QK_TMul.io.results.ready := true.B

  io.scores.valid := QK_TMul.io.results.valid
  io.scores.bits := QK_TMul.io.results.bits

  object state extends ChiselEnum {
    val idle, mul, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.mul
      }
    }
    is(state.mul) {
      when(QK_TMul.io.results.valid) {
        // for (i <- 0 until m) {
        //   for (j <- 0 until n) {
        //     // printf(p"QK_TMul.io.matrixA.bits($i)($j): ${QK_TMul.io.matrixA.bits(i)(j)}\n")
        //     // printf(p"QK_TMul.io.matrixB.bits($i)($j): ${QK_TMul.io.matrixB.bits(i)(j)}\n")
        //     printf(p"io.Query.bits($i)($j): ${io.Query.bits(i)(j)}\n")
        //     printf(p"io.Key.bits($i)($j): ${io.Key.bits(i)(j)}\n")
        //   }
        // }
        // for (i <- 0 until m) {
        //   for (j <- 0 until m) {
        //     printf(p"QK_TMul.io.results.bits($i)($j): ${QK_TMul.io.results.bits(i)(j)}\n")
        //   }
        // }
        stateReg := state.done
      }
    }
    is(state.done) {
      readyReg := true.B
      io.scores.valid := true.B
      stateReg := state.idle
    }
  }
}

// AttnScores: use QKGen to get Q and K, then use QKMulTotal to get scores
// input: inputToken: m * k
// input: weightQ: k * n
// input: weightK: k * n
// output: scores: m * m
class AttnScores(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val scores = Decoupled(Vec(m, Vec(m, UInt(config.outputWidth.W))))
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg

  io.scores.valid := false.B
  io.scores.bits := DontCare

  val QKGen = Module(new QKGen(m, k, n, peCount, gemmType))

  QKGen.io.inputToken.valid := io.inputToken.valid
  QKGen.io.inputToken.bits := io.inputToken.bits
  QKGen.io.weightQ.valid := io.weightQ.valid
  QKGen.io.weightQ.bits := io.weightQ.bits
  QKGen.io.weightK.valid := io.weightK.valid
  QKGen.io.weightK.bits := io.weightK.bits
  QKGen.io.Query.ready := true.B
  QKGen.io.Key.ready := true.B

  // val QueryReg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))
  // val KeyReg = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))

  val QKMul = Module(new QKMulTotalWithReg(m, n, peCount, gemmType))

  val scoresReg = Reg(Vec(m, Vec(m, UInt(config.inputWidth.W))))

  QKMul.io.Query.valid := QKGen.io.Query.valid
  QKMul.io.Query.bits := QKGen.io.Query.bits
  // QKMul.io.Query.bits := QueryReg
  QKMul.io.Key.valid := QKGen.io.Key.valid
  QKMul.io.Key.bits := QKGen.io.Key.bits
  // QKMul.io.Key.bits := KeyReg
  QKMul.io.scores.ready := true.B

  // io.scores.valid := QKMul.io.scores.valid
  // io.scores.bits := QKMul.io.scores.bits
  io.scores.valid := false.B
  io.scores.bits := scoresReg

  object state extends ChiselEnum {
    val idle, gen, mul, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        readyReg := false.B
        stateReg := state.gen
      }
    }
    is(state.gen) {
      printf(p"gen:\n")
      when(QKGen.io.Query.valid && QKGen.io.Key.valid) {
        // QueryReg := QKGen.io.Query.bits
        // KeyReg := QKGen.io.Key.bits
        // for (i <- 0 until m) {
        //   for (j <- 0 until n) {
        //     // printf(p"QueryReg($i)($j): ${QueryReg(i)(j)}\n")
        //     // printf(p"KeyReg($i)($j): ${KeyReg(i)(j)}\n")
        //     printf(p"QKGen.io.Query.bits($i)($j): ${QKGen.io.Query.bits(i)(j)}\n")
        //     printf(p"QKGen.io.Key.bits($i)($j): ${QKGen.io.Key.bits(i)(j)}\n")
        //   }
        // }
        stateReg := state.mul
      }
    }
    is(state.mul) {
      printf(p"mul:\n")
      when(QKMul.io.scores.valid) {
        // for (i <- 0 until m) {
        //   for (j <- 0 until n) {
        //     printf(p"QKMul.io.Query.bits($i)($j): ${QKMul.io.Query.bits(i)(j)}\n")
        //     printf(p"QKMul.io.Key.bits($i)($j): ${QKMul.io.Key.bits(i)(j)}\n")
        //   }
        // }
        // for (i <- 0 until m) {
        //   for (j <- 0 until m) {
        //     printf(p"QKMul.io.scores.bits($i)($j): ${QKMul.io.scores.bits(i)(j)}\n")
        //   }
        // }
        scoresReg := QKMul.io.scores.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      readyReg := true.B
      io.scores.valid := true.B
      stateReg := state.idle
    }
  }

}

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
    val AttnWeights = Flipped(Decoupled(Vec(m, Vec(m, UInt(config.inputWidth.W)))))
    val Value = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val AttnOut = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W))))
  })

  val dataValid = io.AttnWeights.valid && io.Value.valid

  val readyReg = RegInit(true.B)
  io.AttnWeights.ready := readyReg
  io.Value.ready := readyReg
  io.AttnOut.valid := false.B
  io.AttnOut.bits := DontCare

  val ValueMul = Module(new GEMMFMATotal(m, m, n, peCount, gemmType))

  ValueMul.io.matrixA.valid := io.AttnWeights.valid
  ValueMul.io.matrixA.bits := io.AttnWeights.bits
  ValueMul.io.matrixB.valid := io.Value.valid
  ValueMul.io.matrixB.bits := io.Value.bits
  ValueMul.io.results.ready := true.B

  io.AttnOut.valid := ValueMul.io.results.valid
  io.AttnOut.bits := ValueMul.io.results.bits

}

// OutValue: get the final output value
// input: one row of AttnWeights: 1 * m ,total m rows
// input: Value: m * n
// output: one row of AttnOut: 1 * n ,total m rows
// output: done: Bool
class OutValueSingle(
  val m:        Int,
  val n:        Int,
  val peCount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val currentAttnW = Flipped(Decoupled(new currentRowIndex(m, m)))
    val Value = Flipped(Decoupled(Vec(m, Vec(n, UInt(config.inputWidth.W)))))
    val currentAttnO = Decoupled(new currentRowIndex(m, n))
    val done = Output(Bool())
  })

  val dataValid = io.currentAttnW.valid && io.Value.valid

  val readyReg = RegInit(true.B)
  io.currentAttnW.ready := readyReg
  io.Value.ready := readyReg
  io.currentAttnO.valid := false.B
  io.currentAttnO.bits := DontCare
  io.done := false.B

  val multiFMA = Module(new MultiFMA(m, peCount, gemmType))

  val rowIndex = Counter(m)
  val colIndex = Counter(n / peCount)

  multiFMA.io.matrixA_row.valid := io.currentAttnW.valid
  multiFMA.io.matrixA_row.bits := io.currentAttnW.bits.value

  multiFMA.io.matrixB_cols.valid := io.Value.valid
  multiFMA.io.matrixB_cols.bits := VecInit(Seq.tabulate(m) { j =>
    VecInit(Seq.tabulate(peCount) { i =>
      io.Value.bits(j)((colIndex.value * peCount.U + i.U) % n.U)
    })
  }) //m * peCount size block of Value

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
        // readyReg := false.B
        stateReg := state.compute
      }
    }

    is(state.compute) {
      multiFMA.io.reset := false.B
      // printf(p"multiFMA.io.matrixA_row.bits: ${multiFMA.io.matrixA_row.bits}\n")
      // printf(p"multiFMA.io.matrixB_cols.bits: ${multiFMA.io.matrixB_cols.bits}\n")

      when(multiFMA.io.blockResult.valid) {
        for (i <- 0 until peCount) {
          currentRowReg(colIndex.value * peCount.U + i.U) := multiFMA.io.blockResult.bits(i)
        }
        stateReg := state.update
      }
    }

    is(state.update) {
      multiFMA.io.reset := true.B
      io.currentAttnO.valid := false.B
      when(colIndex.inc()) {
        io.currentAttnO.valid := true.B
        io.currentAttnO.bits.index := rowIndex.value
        io.currentAttnO.bits.value := currentRowReg
        // readyReg := true.B
        // io.currentAttnO.ready := true.B
        // io.Value.ready := true.B
        when(rowIndex.inc()) {
          stateReg := state.done
        }.otherwise {
          stateReg := state.compute  
          // wait for next row of AttnWeights
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
