package kernel.alu

import chisel3._
import chisel3.util._
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog
import kernel.deprecated.PE

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
  qGen.io.results.ready := false.B

  kGen.io.matrixA.bits := io.inputToken.bits
  kGen.io.matrixA.valid := io.inputToken.valid
  kGen.io.matrixB.bits := io.weightK.bits
  kGen.io.matrixB.valid := io.weightK.valid
  kGen.io.results.ready := false.B

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
      qGen.io.results.ready := true.B
      kGen.io.results.ready := true.B
      when(qGen.io.results.valid && kGen.io.results.valid) {
        Qreg := qGen.io.results.bits
        Kreg := kGen.io.results.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      qGen.io.results.ready := false.B
      kGen.io.results.ready := false.B
      readyReg := true.B
      io.Query.valid := true.B
      io.Key.valid := true.B
      io.Query.bits := Qreg
      io.Key.bits := Kreg
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

  qGen.io.matrixA.bits := io.inputToken.bits
  qGen.io.matrixA.valid := io.inputToken.valid
  qGen.io.matrixB.bits := io.weightQ.bits
  qGen.io.matrixB.valid := io.weightQ.valid
  qGen.io.results.ready := false.B

  kGen.io.matrixA.bits := io.inputToken.bits
  kGen.io.matrixA.valid := io.inputToken.valid
  kGen.io.matrixB.bits := io.weightK.bits
  kGen.io.matrixB.valid := io.weightK.valid
  kGen.io.results.ready := false.B

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
      qGen.io.results.ready := true.B
      kGen.io.results.ready := true.B
      when(qGen.io.results.valid && kGen.io.results.valid) {
        stateReg := state.done
      }
    }
    is(state.done) {
      qGen.io.results.ready := false.B
      kGen.io.results.ready := false.B
      readyReg := true.B
      io.Query.valid := true.B
      io.Key.valid := true.B
      io.Query.bits := qGen.io.results.bits
      io.Key.bits := kGen.io.results.bits
      stateReg := state.idle
    }
  }
}

// QKMulWithReg: use GEMMFMATotal to get scores
// input: Query: m * n
// input: Key: m * n
// output: scores: m * m
class QKMulWithReg(
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
  QK_TMul.io.results.ready := false.B

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
      QK_TMul.io.results.ready := true.B
      when(QK_TMul.io.results.valid) {
        scoresReg := QK_TMul.io.results.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      QK_TMul.io.results.ready := false.B
      readyReg := true.B
      io.scores.valid := true.B
      io.scores.bits := scoresReg
      stateReg := state.idle
    }
  }
}

// QKMul: use GEMMFMATotal to get scores
// input: Query: m * n
// input: Key: m * n
// output: scores: m * m
class QKMul(
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
  QK_TMul.io.results.ready := false.B

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
      QK_TMul.io.results.ready := true.B
      when(QK_TMul.io.results.valid) {
        stateReg := state.done
      }
    }
    is(state.done) {
      QK_TMul.io.results.ready := false.B
      readyReg := true.B
      io.scores.valid := true.B
      io.scores.bits := QK_TMul.io.results.bits
      stateReg := state.idle
    }
  }
}
// QKMulSingle: use GEMMFMASingle to get scores by row
// input: Query: m * n
// input: Key: m * n
// output: scores: 1 * m
// output: done: Bool
class QKMulSingle(
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
    val curRowScores = Decoupled(new curRowIndex(m, m))
    val done = Output(Bool())
  })

  val dataValid = io.Query.valid && io.Key.valid

  val readyReg = RegInit(true.B)
  io.Query.ready := readyReg
  io.Key.ready := readyReg
  io.curRowScores.valid := false.B
  io.curRowScores.bits := DontCare
  io.done := false.B

  val curRowIndexReg = Reg(new curRowIndex(m, m))

  val QK_TMul = Module(new GEMMFMASingle(m, n, m, peCount, gemmType))
  // val QK_TMul = Module(new GEMMSingleQueue(m, n, m, peCount, gemmType))

  QK_TMul.io.matrixA.valid := io.Query.valid
  QK_TMul.io.matrixA.bits := io.Query.bits
  QK_TMul.io.matrixB.valid := io.Key.valid
  QK_TMul.io.matrixB.bits := VecInit(io.Key.bits.transpose.map(VecInit(_)))
  QK_TMul.io.curRow.ready := false.B

  object state extends ChiselEnum {
    val idle, mul, update, done = Value
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
      QK_TMul.io.curRow.ready := true.B
      when(QK_TMul.io.curRow.valid) {
        curRowIndexReg := QK_TMul.io.curRow.bits
        stateReg := state.update
      }
    }
    is(state.update) {
      QK_TMul.io.curRow.ready := false.B
      io.curRowScores.valid := true.B
      io.curRowScores.bits := curRowIndexReg
      when(QK_TMul.io.done) {
        stateReg := state.done
      }.otherwise {
        stateReg := state.mul
      }
    }
    is(state.done) {
      readyReg := true.B
      io.done := true.B
      stateReg := state.idle
    }
  }
}

// AttnScores: use QKGen to get Q and K, then use QKMul to get scores
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

  // val scoresReg = Reg(Vec(m, Vec(m, UInt(config.outputWidth.W))))
  // val QKGen = Module(new QKGenWithReg(m, k, n, peCount, gemmType))
  val QKGen = Module(new QKGen(m, k, n, peCount, gemmType))

  QKGen.io.inputToken.valid := io.inputToken.valid
  QKGen.io.inputToken.bits := io.inputToken.bits
  QKGen.io.weightQ.valid := io.weightQ.valid
  QKGen.io.weightQ.bits := io.weightQ.bits
  QKGen.io.weightK.valid := io.weightK.valid
  QKGen.io.weightK.bits := io.weightK.bits

  QKGen.io.Query.ready := false.B
  QKGen.io.Key.ready := false.B

  // val QKMul = Module(new QKMulWithReg(m, n, peCount, gemmType))
  val QKMul = Module(new QKMul(m, n, peCount, gemmType))

  QKMul.io.Query.valid := QKGen.io.Query.valid
  QKMul.io.Query.bits := QKGen.io.Query.bits
  QKMul.io.Key.valid := QKGen.io.Key.valid
  QKMul.io.Key.bits := QKGen.io.Key.bits
  QKMul.io.scores.ready := false.B

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
      QKGen.io.Query.ready := true.B
      QKGen.io.Key.ready := true.B
      when(QKGen.io.Query.valid && QKGen.io.Key.valid) {
        stateReg := state.mul
      }
    }
    is(state.mul) {
      QKGen.io.Query.ready := false.B
      QKGen.io.Key.ready := false.B
      QKMul.io.scores.ready := true.B
      when(QKMul.io.scores.valid) {
        // scoresReg := QKMul.io.scores.bits
        stateReg := state.done
      }
    }
    is(state.done) {
      QKMul.io.scores.ready := false.B
      readyReg := true.B
      io.scores.valid := true.B
      // io.scores.bits := scoresReg
      io.scores.bits := QKMul.io.scores.bits
      stateReg := state.idle
    }
  }
}

// AttnScoresSingle: use QKGen to get Q and K, get scores by row
// input: inputToken: m * k
// input: weightQ: k * n
// input: weightK: k * n
// output: curRowScores: 1 * m
// output: done: Bool
class AttnScoresSingle(
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
    val curRowScores = Decoupled(new curRowIndex(m, m))
    val done = Output(Bool())
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.curRowScores.valid := false.B
  io.curRowScores.bits := DontCare
  io.done := false.B

  // val QKGen = Module(new QKGenWithReg(m, k, n, peCount, gemmType))
  val QKGen = Module(new QKGen(m, k, n, peCount, gemmType))

  QKGen.io.inputToken.valid := io.inputToken.valid
  QKGen.io.inputToken.bits := io.inputToken.bits
  QKGen.io.weightQ.valid := io.weightQ.valid
  QKGen.io.weightQ.bits := io.weightQ.bits
  QKGen.io.weightK.valid := io.weightK.valid
  QKGen.io.weightK.bits := io.weightK.bits

  QKGen.io.Query.ready := false.B
  QKGen.io.Key.ready := false.B

  val curRowIndexReg = Reg(new curRowIndex(m, m))
  val QKMul = Module(new GEMMFMASingle(m, n, m, peCount, gemmType))

  QKMul.io.matrixA.valid := QKGen.io.Query.valid
  QKMul.io.matrixA.bits := QKGen.io.Query.bits
  QKMul.io.matrixB.valid := QKGen.io.Key.valid
  QKMul.io.matrixB.bits := VecInit(QKGen.io.Key.bits.transpose.map(VecInit(_)))
  QKMul.io.curRow.ready := false.B

  object state extends ChiselEnum {
    val idle, gen, mul, update, done = Value
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
      QKGen.io.Query.ready := true.B
      QKGen.io.Key.ready := true.B
      when(QKGen.io.Query.valid && QKGen.io.Key.valid) {
        stateReg := state.mul
      }
    }
    is(state.mul) {
      QKGen.io.Query.ready := false.B
      QKGen.io.Key.ready := false.B
      QKMul.io.curRow.ready := true.B
      // printf(p"QKGen.io.Query.bits: ${QKGen.io.Query.bits}\n")
      // printf(p"QKGen.io.Key.bits: ${QKGen.io.Key.bits}\n")
      // printf(p"QKMul.io.matrixA.bits: ${QKMul.io.matrixA.bits}\n")
      // printf(p"QKMul.io.matrixB.bits: ${QKMul.io.matrixB.bits}\n")
      when(QKMul.io.curRow.valid) {
        curRowIndexReg := QKMul.io.curRow.bits
        printf(p"QKMul.io.curRow.bits: ${QKMul.io.curRow.bits}\n")
        printf(p"curRowIndexReg: ${curRowIndexReg}\n")
        stateReg := state.update
      }
    }
    is(state.update) {
      QKMul.io.curRow.ready := false.B
      io.curRowScores.valid := true.B
      io.curRowScores.bits := curRowIndexReg
      printf(p"Update curRowIndexReg: ${curRowIndexReg}\n")
      printf(p"Update io.curRowScores.bits: ${io.curRowScores.bits}\n")
      printf(p"Update io.curRowScores.valid: ${io.curRowScores.valid}\n")
      when(QKMul.io.done) {
        stateReg := state.done
      }.otherwise {
        stateReg := state.mul
      }
    }
    is(state.done) {
      readyReg := true.B
      io.done := true.B
      stateReg := state.idle
    }
  }
}

// AttnScoresSingleQueue: use QKGen to get Q and K, get scores by row
// input: inputToken: m * k
// input: weightQ: k * n
// input: weightK: k * n
// input: flush: Bool
// output: curRowScores: 1 * m
// output: done: Bool
class AttnScoresSingleQueue(
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
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val flush = Input(Bool())
    val curRowScores = Decoupled(new curRowIndex(m, m))
    val done = Output(Bool())
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid

  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg
  io.curRowScores.valid := false.B
  io.curRowScores.bits := DontCare
  io.done := false.B

  // val QKGen = Module(new QKGenWithReg(m, k, n, peCount, gemmType))
  val QKGen = Module(new QKGen(m, k, n, peCount, gemmType))

  QKGen.io.inputToken.valid := io.inputToken.valid
  QKGen.io.inputToken.bits := io.inputToken.bits
  QKGen.io.weightQ.valid := io.weightQ.valid
  QKGen.io.weightQ.bits := io.weightQ.bits
  QKGen.io.weightK.valid := io.weightK.valid
  QKGen.io.weightK.bits := io.weightK.bits

  QKGen.io.Query.ready := false.B
  QKGen.io.Key.ready := false.B

  val curRowIndexReg = Reg(new curRowIndex(m, m))

  val QKMul = Module(new GEMMFMASingle(m, n, m, peCount, gemmType))

  QKMul.io.matrixA.valid := QKGen.io.Query.valid
  QKMul.io.matrixA.bits := QKGen.io.Query.bits
  QKMul.io.matrixB.valid := QKGen.io.Key.valid
  QKMul.io.matrixB.bits := VecInit(QKGen.io.Key.bits.transpose.map(VecInit(_)))
  QKMul.io.curRow.ready := false.B

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
  curBuffer.io.flush.get := io.flush
  curBuffer.io.enq <> QKMul.io.curRow
  io.curRowScores <> curBuffer.io.deq

  object state extends ChiselEnum {
    val idle, gen, mul, update, done = Value
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
      QKGen.io.Query.ready := true.B
      QKGen.io.Key.ready := true.B
      when(QKGen.io.Query.valid && QKGen.io.Key.valid) {
        stateReg := state.mul
      }
    }
    is(state.mul) {
      QKGen.io.Query.ready := false.B
      QKGen.io.Key.ready := false.B
      QKMul.io.curRow.ready := true.B
      // printf(p"QKGen.io.Query.bits: ${QKGen.io.Query.bits}\n")
      // printf(p"QKGen.io.Key.bits: ${QKGen.io.Key.bits}\n")
      when(QKMul.io.curRow.valid) {
        // curRowIndexReg := QKMul.io.curRow.bits
        stateReg := state.update
      }
    }
    is(state.update) {
      QKMul.io.curRow.ready := false.B
      io.curRowScores.valid := true.B
      // io.curRowScores.bits := curRowIndexReg
      when(QKMul.io.done) {
        stateReg := state.done
      }.otherwise {
        stateReg := state.mul
      }
    }
    is(state.done) {
      readyReg := true.B
      io.done := true.B
      stateReg := state.idle
    }
  }
}
