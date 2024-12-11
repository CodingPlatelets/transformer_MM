package models.llama3

import common.llamaConfig
import chisel3._
import chisel3.util._
import kernel.alu.GEMM
import kernel.alu.GEMMDataType
import kernel.alu.DataWidthConfig

class metrixController extends Module with llamaConfig {}

class currentSystolicGroupIdx(
  val nk: Int,
  val m:  Int,
  val p:  Int,
  val q:  Int
)(
  implicit config: DataWidthConfig)
    extends Bundle
    with llamaConfig {

  val row = Output(UInt(log2Ceil(m / nk).W))
  val col = Output(UInt(log2Ceil(q / nk).W))
  val value = Output(Vec(nk * nk, UInt(config.inputWidth.W)))
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

  // 初始化输出矩阵
  io.outMatrix.foreach(_ := 0.U)

  for (blockRow <- 0 until numBlocksRow) {
    for (blockCol <- 0 until numBlocksCol) {
      val blockIndex = blockRow * numBlocksCol + blockCol
      val block = io.inBlocks(blockIndex)

      // 解包当前方阵块
      for (i <- 0 until nk) {
        for (j <- 0 until nk) {
          // 计算在输出向量中的位置
          val flatIndex = (blockRow * nk + i) * p + (blockCol * nk + j)
          // 从打包的UInt中提取对应位置的元素
          val elementPos = (nk * nk - 1 - (i * nk + j)) * config.inputWidth
          io.outMatrix(flatIndex) := block(elementPos + config.inputWidth - 1, elementPos)
        }
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

/*
 * matrix mul matrix
 * matrixA is [m, p]
 * matrixB is [p, q]
 * use k^2 systolic-groups with dim as n to do the matrix mul
 * designed for QKV generation, but has a output for current systolic group idx
 */
class GenerationMatrixMul(
  val k:        Int,
  val n:        Int,
  val m:        Int,
  val p:        Int,
  val q:        Int,
  val gemmType: GEMMDataType.Type
)(
  implicit config: DataWidthConfig)
    extends Module
    with llamaConfig {
  // param check
  implicit val nk: Int = k * n
  require(m % nk == 0)
  require(p % nk == 0)
  require(q % nk == 0)

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(m * p, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(p * q, UInt(config.inputWidth.W))))
    val result = Decoupled(Vec(m * q, UInt(config.outputWidth.W)))
    val current = ValidIO(new currentSystolicGroupIdx(nk, m, p, q))
    val reset = Input(Bool())
  })

  // reshape the input data as block => [rows, cols] [nk, nk]
  val matrixAReshape = RegInit(MatrixSplit(m, p, nk)(io.in_a.bits))
  val matrixBReshape = RegInit(MatrixSplit(p, q, nk)(io.in_b.bits))

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
  val validReg = RegInit(false.B)
  io.result.valid := validReg
  val dataShapedValid = RegInit(false.B)
  gemmGroup.io.in_a.valid := dataShapedValid
  gemmGroup.io.in_b.valid := dataShapedValid

  io.current.valid := false.B
  io.current.bits := DontCare

  val blockAIdx = rowIdx.value * middle.U + calTimes.value
  val blockBIdx = calTimes.value * cols.U + colIdx.value
  val gemmInputA = matrixAReshape(blockAIdx).asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))
  val gemmInputB = matrixBReshape(blockBIdx).asTypeOf(Vec(nk * nk, UInt(config.inputWidth.W)))

  val unShapedResult = RegInit(VecInit.fill(m * q / nk / nk)(0.U((nk * nk * config.outputWidth).W)))
  io.result.bits := MatrixRestore(m, q, nk)(unShapedResult)

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
        // collect the result of the [rowIdx, colIdx] block
        val afterRowLine = gemmGroup.io.out.bits
        unShapedResult(rowIdx.value * cols.U + colIdx.value) := afterRowLine.asTypeOf(
          UInt((nk * nk * config.outputWidth).W)
        )

        // send the current systolic group idx
        io.current.valid := true.B
        io.current.bits.row := rowIdx.value
        io.current.bits.col := colIdx.value
        io.current.bits.value := afterRowLine

        val isRowEnd = colIdx.inc()
        stateReg := Mux(rowIdx.inc() && isRowEnd, state.done, state.cal)
      }
    }

    is(state.done) {
      validReg := true.B
      when(io.result.ready) {
        validReg := false.B
        stateReg := state.idle
        readyReg := true.B
      }
    }
  }
}
