package kernel.alu

import chisel3._
import chisel3.util._
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog

class currentRowIndex(
  val m: Int,
  val n: Int
)(
  implicit config: DataWidthConfig)
    extends Bundle {
  val index = Output(UInt(log2Ceil(m).W)) //输出的行索引
  val value = Output(Vec(n, UInt(config.outputWidth.W))) //输出的行值
}

class MultiFMAMM(
  val k:        Int = 64,
  val PECount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  val io = IO(new Bundle {
    val matrixA_row = Input(Vec(k, UInt(config.inputWidth.W))) // 矩阵1的一行
    val matrixB_cols = Input(Vec(PECount, Vec(k, UInt(config.inputWidth.W)))) // 矩阵2的PECount列向量
    val results = Output(Vec(PECount, UInt(config.outputWidth.W))) // 点积结果
    val valids = Output(Vec(PECount, Bool())) // 结果有效标志
    val reset = Input(Bool())
  })

  // 创建 PECount 个 PE 实例
  val pes = Seq.fill(PECount)(gemmType match {
    case GEMMDataType.Fxp  => Module(new PEFxp()).io
    case GEMMDataType.Fp32 => Module(new PEFp()).io
    case GEMMDataType.Fp64 => Module(new PEFp()).io
    case _                 => throw new IllegalArgumentException("Unsupported GEMM type")
  })

  // 当前索引寄存器
  val index = RegInit(0.U(log2Ceil(k).W))
  // 结果有效标志
  val valid = RegInit(false.B)

  // 连接每个 PE 的输入和输出
  for (i <- 0 until PECount) {
    val pe = pes(i)
    pe.reset := io.reset
    pe.in_h := io.matrixA_row(index) // 矩阵1的当前行值
    pe.in_v := io.matrixB_cols(i)(index) // 矩阵2的第i列当前值
    io.results(i) := pe.out // 输出结果
    io.valids(i) := valid // 当点积完成时置位 valid
  }

  // 索引、结果有效位控制逻辑
  when(io.reset) {
    index := 0.U
    valid := false.B
  }.elsewhen(index =/= (k - 1).U) {
    index := index + 1.U
    valid := false.B
  }.elsewhen(index === (k - 1).U) {
    index := 0.U
    valid := true.B
  }
}

class GEMMFMA(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val PECount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  // require(m % PECount == 0 && k % PECount == 0 && n % PECount == 0, "Matrix dimensions must be divisible by PECount")
  val io = IO(new Bundle {
    val matrixA = Input(Vec(m, Vec(k, UInt(config.inputWidth.W)))) // 矩阵A
    val matrixB = Input(Vec(k, Vec(n, UInt(config.inputWidth.W)))) // 矩阵B
    val results = Output(Vec(m, Vec(n, UInt(config.outputWidth.W)))) // 结果矩阵
    val done = Output(Bool()) // 完成标志
  })

  val rowIndex = RegInit(0.U(log2Ceil(m).W)) // 当前行索引
  val colIndex = RegInit(0.U(log2Ceil(n).W)) // 当前列块索引
  val resultMatrix = Reg(Vec(m, Vec(n, UInt(config.outputWidth.W)))) // 存储结果
  val doneFlag = RegInit(false.B) // 完成标志
  val resetFlag = RegInit(false.B) // 复位标志

  // 实例化MultiFMA
  val multiFMA = Module(new MultiFMAMM(k, PECount, gemmType))

  // 输入连接
  multiFMA.io.matrixA_row := io.matrixA(rowIndex)
  multiFMA.io.matrixB_cols := VecInit(Seq.tabulate(PECount) { i =>
    VecInit(io.matrixB.map(_(colIndex + i.U)))
  })

  // 一块处理结束, 结果存储, 更新块索引
  when(multiFMA.io.valids.reduce(_ && _)) {
    for (i <- 0 until PECount) {
      resultMatrix(rowIndex)(colIndex + i.U) := multiFMA.io.results(i)
    }
    resetFlag := true.B
    // 更新块索引
    when(colIndex === (n - PECount).U) {
      colIndex := 0.U
      when(rowIndex === (m - 1).U) {
        rowIndex := 0.U
        doneFlag := true.B
      }.otherwise {
        rowIndex := rowIndex + 1.U
      }
    }.otherwise {
      colIndex := colIndex + PECount.U
    }
  }

  // 复位控制，也即清空累加器
  when(resetFlag) {
    multiFMA.io.reset := true.B
    resetFlag := false.B
  }.otherwise {
    multiFMA.io.reset := false.B
  }

  io.done := doneFlag
  io.results := resultMatrix
}

class GEMMFMATotal(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val PECount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  require(m % PECount == 0 && k % PECount == 0 && n % PECount == 0, "Matrix dimensions must be divisible by PECount")
  val io = IO(new Bundle {
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W))))) // 矩阵A
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W))))) // 矩阵B
    val results = Decoupled(Vec(m, Vec(n, UInt(config.outputWidth.W)))) // 结果矩阵
  })

  val rowIndex = Counter(m)
  val colIndex = Counter(n / PECount)

  val resultsReg = Reg(Vec(m, Vec(n, UInt(config.outputWidth.W)))) // 存储结果
  val resetFlag = RegInit(false.B) // 复位标志，用于清空MultiFMA

  val dataValid = io.matrixA.valid && io.matrixB.valid
  val readyReg = RegInit(true.B)
  io.matrixA.ready := readyReg
  io.matrixB.ready := readyReg

  io.results.valid := false.B
  io.results.bits := DontCare

  // 实例化MultiFMA
  val multiFMA = Module(new MultiFMAMM(k, PECount, gemmType))

  // 输入连接
  multiFMA.io.matrixA_row := io.matrixA.bits(rowIndex.value)
  multiFMA.io.matrixB_cols := VecInit(Seq.tabulate(PECount) { i =>
    VecInit(io.matrixB.bits.map(_((colIndex.value * PECount.U + i.U) % n.U)))
  })

  // 状态机定义
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
      when(multiFMA.io.valids.reduce(_ && _)) {
        for (i <- 0 until PECount) {
          resultsReg(rowIndex.value)((colIndex.value * PECount.U + i.U) % n.U) := multiFMA.io.results(i)
        }
        resetFlag := true.B
        stateReg := state.update
      }
    }

    is(state.update) {
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
  when(resetFlag) {
    multiFMA.io.reset := true.B
    resetFlag := false.B
  }.otherwise {
    multiFMA.io.reset := false.B
  }

}
class GEMMFMASingle(
  val m:        Int,
  val k:        Int,
  val n:        Int,
  val PECount:  Int = 16,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {
  require(m % PECount == 0 && k % PECount == 0 && n % PECount == 0, "Matrix dimensions must be divisible by PECount")
  val io = IO(new Bundle {
    val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W))))) // 矩阵A
    val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W))))) // 矩阵B
    val currentRow = Decoupled(new currentRowIndex(m, n)) //输出的行索引
    val done = Output(Bool()) // 整个矩阵完成标志
  })

  val rowIndex = Counter(m)
  val colIndex = Counter(n / PECount)

  val currentRowReg = Reg(Vec(n, UInt(config.outputWidth.W))) // 存储当前行结果
  val doneFlag = RegInit(false.B) // 完成标志
  val resetFlag = RegInit(false.B) // 复位标志

  val dataValid = io.matrixA.valid && io.matrixB.valid
  val readyReg = RegInit(true.B)
  io.matrixA.ready := readyReg
  io.matrixB.ready := readyReg

  io.currentRow.valid := false.B
  io.currentRow.bits := DontCare

  // 实例化MultiFMA
  val multiFMA = Module(new MultiFMAMM(k, PECount, gemmType))

  // 输入连接
  multiFMA.io.matrixA_row := io.matrixA.bits(rowIndex.value)
  multiFMA.io.matrixB_cols := VecInit(Seq.tabulate(PECount) { i =>
    VecInit(io.matrixB.bits.map(_((colIndex.value * PECount.U + i.U) % n.U)))
  })

  // 状态机定义
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
      when(multiFMA.io.valids.reduce(_ && _)) {
        for (i <- 0 until PECount) {
          currentRowReg((colIndex.value * PECount.U + i.U) % n.U) := multiFMA.io.results(i)
        }
        resetFlag := true.B
        stateReg := state.update
      }
    }

    is(state.update) {
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
      doneFlag := true.B
      readyReg := true.B
      stateReg := state.idle
    }
  }
  when(resetFlag) {
    multiFMA.io.reset := true.B
    resetFlag := false.B
  }.otherwise {
    multiFMA.io.reset := false.B
  }
  io.done := doneFlag
}

// TODO: 优化,bug
class QKMulFMA(
  val m:              Int,
  val k:              Int,
  val n:              Int,
  val PECount1:       Int = 16,
  val PECount2:       Int = 16,
  val gemmType:       GEMMDataType.Type,
  val bufferSizeGemm: Int = 32
)(
  implicit config: DataWidthConfig)
    extends Module
    with DebugLog {

  class QKGenderWarper(
    val m:          Int,
    val k:          Int,
    val n:          Int,
    val PECount:    Int = 16,
    val gemmType:   GEMMDataType.Type,
    val bufferSize: Int
  )(
    implicit config: DataWidthConfig)
      extends Module
      with DebugLog {
    val io = IO(new Bundle {
      val matrixA = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W))))) // 矩阵A
      val matrixB = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W))))) // 矩阵B
      val flush = Input(Bool())
      val outMatrix = Decoupled(new currentRowIndex(m, n))
    })

    val qkGenMul = Module(new GEMMFMASingle(m, k, n, PECount, gemmType))
    io.matrixA <> qkGenMul.io.matrixA
    io.matrixB <> qkGenMul.io.matrixB

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

    // hasFlush must be true
    currentBuffer.io.flush.get := io.flush

    // ATTENTION: we assert the size of the buffer is huge enough to hold the current systolic group output
    // we ignore the ready signal of the enq
    currentBuffer.io.enq.bits := qkGenMul.io.currentRow.bits
    currentBuffer.io.enq.valid := qkGenMul.io.currentRow.valid

    io.outMatrix <> currentBuffer.io.deq
  }

  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m, Vec(k, UInt(config.inputWidth.W)))))
    val weightQ = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val weightK = Flipped(Decoupled(Vec(k, Vec(n, UInt(config.inputWidth.W)))))
    val score = Decoupled(Vec(m, Vec(m, UInt(config.outputWidth.W))))
    val resetBuffer = Input(Bool())
  })

  val dataValid = io.inputToken.valid && io.weightQ.valid && io.weightK.valid
  val readyReg = RegInit(true.B)
  io.inputToken.ready := readyReg
  io.weightQ.ready := readyReg
  io.weightK.ready := readyReg

  // QKGen，Q: m * n, K: m * n
  val qGen = Module(new QKGenderWarper(m, k, n, PECount1, gemmType, bufferSizeGemm))
  val kGen = Module(new QKGenderWarper(m, k, n, PECount1, gemmType, bufferSizeGemm))

  qGen.io.matrixA <> io.inputToken
  qGen.io.matrixB <> io.weightQ
  kGen.io.matrixA <> io.inputToken
  kGen.io.matrixB <> io.weightK

  qGen.io.flush := io.resetBuffer
  kGen.io.flush := io.resetBuffer

  // // QKMul Q*K^T, Q: m * n, K: m * n -> m * m
  // val Qrow = qGen.io.outMatrix.bits.value // one row of Q: 1 * n
  // val Krow = kGen.io.outMatrix.bits.value // one row of K: 1 * n
  // val QIndex = qGen.io.outMatrix.bits.index // the index of Q row
  // val KIndex = kGen.io.outMatrix.bits.index // the index of K row

  // 创建一个 MultiFMAMM 模块来计算 Q 的一行和 K 的多列的乘积结果 中间维度为n
  // val multiFMA = Module(new MultiFMAMM(n, PECount2, gemmType))

  val qQueue = Module(new Queue(new currentRowIndex(m, n), bufferSizeGemm))
  val kQueue = Module(new Queue(new currentRowIndex(m, n), bufferSizeGemm))

  // 将生成的每一行数据存储到队列中
  qQueue.io.enq.bits := qGen.io.outMatrix.bits
  qQueue.io.enq.valid := qGen.io.outMatrix.valid
  kQueue.io.enq.bits := kGen.io.outMatrix.bits
  kQueue.io.enq.valid := kGen.io.outMatrix.valid
// 创建一个 M*N 的寄存器组来保存所有的 Q 和 K 值
  val qMatrix = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))
  val k_TMatrix = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))

  // 状态机定义
  object state extends ChiselEnum {
    val idle, load, compute, done = Value
  }
  val stateReg = RegInit(state.idle)

  // 计数器，用于跟踪 Q 和 K 的行数
  val qCounter = RegInit(0.U(log2Ceil(m).W))
  val kCounter = RegInit(0.U(log2Ceil(m).W))

  // 创建一个 M*M 的寄存器组来保存所有的结果
  val scoreValue = RegInit(VecInit(Seq.fill(m)(VecInit(Seq.fill(m)(0.U(config.outputWidth.W))))))

  switch(stateReg) {
    is(state.idle) {
      when(qQueue.io.enq.valid && kQueue.io.enq.valid) {
        stateReg := state.load
      }
    }

    is(state.load) {
      when(qQueue.io.deq.valid && kQueue.io.deq.valid) {
        qMatrix(qQueue.io.deq.bits.index) := qQueue.io.deq.bits.value
        for (i <- 0 until n) {
          k_TMatrix(i)(kQueue.io.deq.bits.index) := kQueue.io.deq.bits.value(i) // 将 K 的值存储到转置后的 kMatrix 中
        }
        qCounter := qCounter + 1.U
        kCounter := kCounter + 1.U
        qQueue.io.deq.ready := true.B
        kQueue.io.deq.ready := true.B
        when(qCounter === (m - 1).U && kCounter === (m - 1).U) {
          stateReg := state.compute
        }
      }
    }

    is(state.compute) {
      val multiFMA = Module(new GEMMFMA(m, n, m, PECount2, gemmType))
      multiFMA.io.matrixA := qMatrix
      multiFMA.io.matrixB := k_TMatrix
      io.score.bits := multiFMA.io.results
      io.score.valid := multiFMA.io.done
      stateReg := state.done
    }

    is(state.done) {
      // 完成标志
      io.score.valid := true.B
      stateReg := state.idle
    }
  }

  when(io.resetBuffer) {
    qCounter := 0.U
    kCounter := 0.U
    scoreValue := VecInit(Seq.fill(m)(VecInit(Seq.fill(m)(0.U(config.outputWidth.W)))))
    stateReg := state.idle
  }
  // // 创建一个 M*N 的寄存器组来保存所有的 K 值
  // val kMatrix = Reg(Vec(m, Vec(n, UInt(config.inputWidth.W))))

  // // 从队列中提取 Q 的一行和 K 的多列
  // val qRowFromQueue = qQueue.io.deq.bits.value
  // val qIndexFromQueue = qQueue.io.deq.bits.index

  // val kColsFromQueue = Reg(Vec(PECount2, Vec(n, UInt(config.inputWidth.W))))
  // val kIndexFromQueue = Reg(Vec(PECount2, UInt(log2Ceil(m).W)))

  // // 计数器，用于跟踪 K 的列数
  // val kCounter = RegInit(0.U(log2Ceil(n / PECount2).W))

  // // 当 K 队列中有足够的列时，提取 K 的多列
  // when(kQueue.io.deq.valid && kCounter < PECount2.U) {
  //   kColsFromQueue(kCounter) := kQueue.io.deq.bits.value
  //   kIndexFromQueue(kCounter) := kQueue.io.deq.bits.index
  //   kMatrix(kQueue.io.deq.bits.index) := kQueue.io.deq.bits.value
  //   kCounter := kCounter + 1.U
  //   kQueue.io.deq.ready := true.B
  // }.otherwise {
  //   kQueue.io.deq.ready := false.B
  // }

  // // 当 K 队列中有足够的列时，进行矩阵乘法
  // when(kCounter === PECount2.U) {
  //   multiFMA.io.matrixA_row := qRowFromQueue
  //   multiFMA.io.matrixB_cols := kColsFromQueue
  //   kCounter := 0.U
  // }

  // // 连接结果和有效标志
  // val scoreValue = RegInit(VecInit(Seq.fill(m)(VecInit(Seq.fill(m)(0.U(config.outputWidth.W))))))
  // for (i <- 0 until PECount2) {
  //   when(multiFMA.io.valids(i)) {
  //     scoreValue(qIndexFromQueue)(kIndexFromQueue(i)) := multiFMA.io.results(i)
  //   }
  // }

  // io.score.bits := scoreValue
  // io.score.valid := qQueue.io.deq.valid && kQueue.io.deq.valid

  // // 当 qQueue 继续有值时，继续处理
  // when(qQueue.io.deq.valid && kQueue.io.deq.valid) {
  //   qQueue.io.deq.ready := true.B
  // }.otherwise {
  //   qQueue.io.deq.ready := false.B
  // }

  // when(io.resetBuffer) {
  //   kCounter := 0.U
  //   scoreValue := VecInit(Seq.fill(m)(VecInit(Seq.fill(m)(0.U(config.outputWidth.W)))))
  // }

  // // final result idx
  // val rowIdx = RegInit(0.U(log2Ceil(m / PECount2).W))
  // val colIdx = RegInit(0.U(log2Ceil(m / PECount2).W))
  // val resValid = RegInit(false.B)
  // io.score.valid := resValid

  // io.score.bits := scoreValue

  // when(resValid && io.score.ready) {
  //   resValid := false.B
  // }

}
