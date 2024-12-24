package models.llama3

import common.llamaConfig
import chisel3._
import chisel3.util._
import kernel.alu.GEMM
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig
import kernel.utils.DebugLog
class metrixController extends Module with llamaConfig {}

/*
 * current systolic group idx
 * @param nk: systolic group dim
 * @param m: left matrix rows
 * @param q: right matrix rows
 */
class currentSystolicGroupIdx(
)(
  implicit config: DataWidthConfig)
    extends Bundle
    with llamaConfig {
  val nk: Int = systolicSizeGen * systolicGroupSizeGen
  val row = Output(UInt(log2Ceil(m / nk).W))
  val col = Output(UInt(log2Ceil(q / nk).W))
  val value = Output(UInt((nk * nk * config.inputWidth).W))
}

class MatrixSplit(
  val m:  Int,
  val p:  Int,
  val nk: Int
)(
  implicit config: DataWidthConfig)
    extends Module {
  require(m % nk == 0 && p % nk == 0, "m and p must be divisible by nk")

  val io = IO(new Bundle {
    val inMatrix = Input(Vec(m * p, UInt(config.inputWidth.W)))
    val outBlocks = Output(Vec(m / nk * p / nk, UInt((nk * nk * config.inputWidth).W)))
  })

  val numBlocksRow = m / nk
  val numBlocksCol = p / nk

  for (blockRow <- 0 until numBlocksRow) {
    for (blockCol <- 0 until numBlocksCol) {
      val blockIndex = blockRow * numBlocksCol + blockCol

      // 收集当前方阵块的所有元素
      val blockElements = for {
        i <- 0 until nk
        j <- 0 until nk
      } yield {
        // 计算在一维输入向量中的索引
        val flatIndex = (blockRow * nk + i) * p + (blockCol * nk + j)
        io.inMatrix(flatIndex)
      }

      // 将方阵元素连接成一个UInt
      io.outBlocks(blockIndex) := VecInit(blockElements).asUInt
    }
  }
}

object MatrixSplit {
  def apply(
    m:        Int,
    p:        Int,
    nk:       Int
  )(inMatrix: Vec[UInt]
  )(
    implicit config: DataWidthConfig
  ): Vec[UInt] = {
    val newMatrixSplit = Module(new MatrixSplit(m, p, nk))
    newMatrixSplit.io.inMatrix := inMatrix
    newMatrixSplit.io.outBlocks
  }
}

class MatrixRestore(
  val m:  Int,
  val p:  Int,
  val nk: Int
)(
  implicit config: DataWidthConfig)
    extends Module {
  require(m % nk == 0 && p % nk == 0, "m and p must be divisible by nk")

  val io = IO(new Bundle {
    // 输入是打包的方阵序列
    val inBlocks = Input(Vec(m / nk * p / nk, UInt((nk * nk * config.inputWidth).W)))
    // 输出是一维向量表示的矩阵
    val outMatrix = Output(Vec(m * p, UInt(config.inputWidth.W)))
  })

  val numBlocksRow = m / nk
  val numBlocksCol = p / nk

  // initialize the output matrix
  io.outMatrix.foreach(_ := 0.U)

  for (blockRow <- 0 until numBlocksRow) {
    for (blockCol <- 0 until numBlocksCol) {
      val blockIndex = blockRow * numBlocksCol + blockCol
      val block = io.inBlocks(blockIndex).asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))

      // 解包当前方阵块
      for {
        i <- 0 until nk
        j <- 0 until nk
      } {
        io.outMatrix((blockRow * nk + i) * p + (blockCol * nk + j)) := block(i * nk + j)
      }
    }
  }
}

object MatrixRestore {
  def apply(
    m:        Int,
    p:        Int,
    nk:       Int
  )(inBlocks: Vec[UInt]
  )(
    implicit config: DataWidthConfig
  ): Vec[UInt] = {
    val newMatrixRestore = Module(new MatrixRestore(m, p, nk))
    newMatrixRestore.io.inBlocks := inBlocks
    newMatrixRestore.io.outMatrix
  }
}

class BlockMatrixRestore(
  val nk: Int
)(
  implicit config: DataWidthConfig)
    extends Module {
  val io = IO(new Bundle {
    val inBlocks = Input(UInt((nk * nk * config.inputWidth).W))
    val outMatrix = Output(Vec(nk * nk, UInt(config.inputWidth.W)))
  })

  io.outMatrix := io.inBlocks.asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))
}

object BlockMatrixRestore {
  def apply(
    nk:       Int
  )(inBlocks: UInt
  )(
    implicit config: DataWidthConfig
  ): Vec[UInt] = {
    val newBlockMatrixRestore = Module(new BlockMatrixRestore(nk))
    newBlockMatrixRestore.io.inBlocks := inBlocks
    newBlockMatrixRestore.io.outMatrix
  }
}

/*
 * matrix mul matrix
 * matrixA is [m, p]
 * matrixB is [p, q]
 * use k^2 systolic-groups with dim as n to do the matrix mul
 * designed for QKV generation, but has a output for current systolic group idx
 */
class GenerationMatrixMul(
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with llamaConfig
    with DebugLog {
  // param check
  implicit val nk: Int = systolicSizeGen * systolicGroupSizeGen
  require(m % nk == 0)
  require(p % nk == 0)
  require(q % nk == 0)

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(m * p, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
    val current = ValidIO(new currentSystolicGroupIdx)
    val reset = Input(Bool())
  })

  // reshape the input data as block => [rows, cols] [nk, nk]
  val matrixAReshape = RegInit(VecInit.fill(m / nk * p / nk)(0.U((nk * nk * config.inputWidth).W)))
  val matrixBReshape = RegInit(VecInit.fill(p / nk * q / nk)(0.U((nk * nk * config.inputWidth).W)))
  matrixAReshape := MatrixSplit(m, p, nk)(io.in_a.bits)
  matrixBReshape := MatrixSplit(p, q, nk)(io.in_b.bits)

  // debugLog(p"matrixAReshape: ${matrixAReshape}\n", LogLevel.DEBUG)
  // debugLog(p"matrixBReshape: ${matrixBReshape}\n", LogLevel.DEBUG)

  // systolic alu
  val gemmGroup = Module(new GEMM(nk, gemmType))

  // systolic group idx
  val rows = m / nk
  val cols = q / nk
  val middle = p / nk
  val rowIdx = Counter(rows)
  val colIdx = Counter(cols)
  val calTimes = Counter(middle)

  val dataValid = io.in_a.valid && io.in_b.valid
  val readyReg = RegInit(true.B)
  io.in_a.ready := readyReg
  io.in_b.ready := readyReg
  val dataShapedValid = RegInit(false.B)
  gemmGroup.io.in_a.valid := dataShapedValid
  gemmGroup.io.in_b.valid := dataShapedValid

  io.current.valid := false.B
  io.current.bits := DontCare

  val blockAIdx = rowIdx.value * middle.U + calTimes.value
  val blockBIdx = calTimes.value * cols.U + colIdx.value
  val gemmInputA = matrixAReshape(blockAIdx).asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))
  val gemmInputB = matrixBReshape(blockBIdx).asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))

  gemmGroup.io.in_a.bits := gemmInputA
  gemmGroup.io.in_b.bits := gemmInputB
  gemmGroup.io.reset := false.B

  val gemmGroupReady = RegInit(false.B)
  gemmGroup.io.out.ready := gemmGroupReady

  object state extends ChiselEnum {
    val idle, cal, collect, done = Value
  }
  val stateReg = RegInit(state.idle)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        dataShapedValid := true.B
        stateReg := state.cal
        readyReg := false.B
      }
    }

    is(state.cal) {
      // acc mode
      gemmGroup.io.reset := false.B
      // when a gemm block is done, io.current will send data
      gemmGroup.io.out.ready := true.B
      when(gemmGroup.io.out.valid) {
        val isfinal = calTimes.inc()
        when(isfinal) {
          // 当这是最后一个值的时候，不要消费这个值
          gemmGroup.io.out.ready := false.B
          stateReg := state.collect
          gemmGroupReady := false.B
        }
      }
    }

    is(state.collect) {
      // collect mode
      gemmGroup.io.reset := false.B
      gemmGroupReady := true.B
      // still has the last gemm block to cal
      when(gemmGroup.io.out.valid) {
        gemmGroup.io.reset := true.B
        gemmGroup.io.out.ready := true.B

        // collect the result of the [rowIdx, colIdx] block
        val afterRowLine = gemmGroup.io.out.bits

        // send the current systolic group idx
        io.current.valid := true.B
        io.current.bits.row := rowIdx.value
        io.current.bits.col := colIdx.value
        io.current.bits.value := afterRowLine.asUInt

        val isRowEnd = colIdx.inc()
        when(!isRowEnd) {
          stateReg := state.cal
        }.otherwise {
          val isAllEnd = rowIdx.inc()
          when(!isAllEnd) {
            stateReg := state.cal
          }.otherwise {
            dataShapedValid := false.B
            stateReg := state.done
          }
        }
      }
    }

    is(state.done) {
      stateReg := state.idle
      readyReg := true.B
    }
  }

  debugLog(
    p"stateReg: $stateReg,\t currentValid: ${io.current.valid},\t rowIdx: ${rowIdx.value},\t colIdx: ${colIdx.value},\t gemmValid: ${gemmGroup.io.out.valid}\n"
  )
}

/*
 * using two GenerationMatrixMul Modules(as QKGEN) to do q,k generation simultaneously.
 * using another GenerationMatrixMul Module(as QKMUL) to do q,k mul.
 * the output of QKMUL is the final result.
 * using the output of QKGEN to Stitch the final result.
 * the k1,n1 are for q,k generation, the k2,n2 are for q,k mul.
 */
class QKMul(
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with llamaConfig
    with DebugLog {

  val nk1: Int = systolicSizeGen * systolicGroupSizeGen
  val nk2: Int = systolicSizeMul * systolicGroupSizeMul
  require(m % nk1 == 0)
  require(p % nk1 == 0)
  require(q % nk1 == 0)
  require(m % nk2 == 0)
  require(q % nk2 == 0)

  class QKGenerationMatrixMulWarper(
    val gemmType:   GEMMDataType.Type,
    val bufferSize: Int
  )(
    implicit config: DataWidthConfig)
      extends Module
      with llamaConfig
      with DebugLog {
    val io = IO(new Bundle {
      val in_a = Flipped(Decoupled(Vec(m * p, UInt(config.inputWidth.W))))
      val in_b = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
      val flush = Input(Bool())
      val outMatrix = Decoupled(new currentSystolicGroupIdx)
    })

    val qkGenMul = Module(new GenerationMatrixMul(gemmType))
    io.in_a <> qkGenMul.io.in_a
    io.in_b <> qkGenMul.io.in_b

    val currentBuffer = Module(
      new Queue(
        new currentSystolicGroupIdx,
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
    currentBuffer.io.enq.bits := qkGenMul.io.current.bits
    currentBuffer.io.enq.valid := qkGenMul.io.current.valid

    io.outMatrix <> currentBuffer.io.deq
  }

  val io = IO(new Bundle {
    val inputToken = Flipped(Decoupled(Vec(m * p, UInt(config.inputWidth.W))))
    val weightQ = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
    val weightK = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
    val score = Decoupled(Vec(m * q, UInt(config.inputWidth.W)))
    val resetBuffer = Input(Bool())
  })

  val qGen = new QKGenerationMatrixMulWarper( gemmType, bufferSizeGemm)
  val kGen = new QKGenerationMatrixMulWarper( gemmType, bufferSizeGemm)

  qGen.io.in_a <> io.inputToken
  qGen.io.in_b <> io.weightQ
  kGen.io.in_a <> io.inputToken
  kGen.io.in_b <> io.weightQ

  qGen.io.flush := io.resetBuffer
  kGen.io.flush := io.resetBuffer

  // final result idx
  val rowIdx = RegInit(0.U(log2Ceil(m / nk2).W))
  val colIdx = RegInit(0.U(log2Ceil(m / nk2).W))
  val resValid = RegInit(false.B)
  io.score.valid := resValid

  val scoreValue = RegInit(VecInit.fill(m * q)(0.U(config.outputWidth.W)))
  io.score.bits := scoreValue

  when(resValid && io.score.ready) {
    resValid := false.B
  }

}

class GemmPool(
  val n:        Int,
  val poolSize: Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with llamaConfig
    with DebugLog {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new currentSystolicGroupIdx))
  })

}
