package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import fputil.FPMult
import fputil.FPAdd
import hardfloat._

// Compute A * B = C, where A is p*m, B is m*q, C is p*q 
class CustomGemm (val n: Int = 4, val p: Int = 512, val q: Int = 512, val m: Int = 128, val gemmType: GEMMDataType.Type) (implicit config: DataWidthConfig)
    extends Module
    with GEMMAccuracyConfig
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(p * m, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(m * q, UInt(config.inputWidth.W))))
    val out = Decoupled(Vec(p * q, UInt(config.outputWidth.W)))
    val reset = Input(Bool())
  })
  val matrixAReg = RegInit(VecInit.fill(p)(VecInit.fill(m)(0.U(config.inputWidth.W))))
  val matrixBReg = RegInit(VecInit.fill(m)(VecInit.fill(q)(0.U(config.inputWidth.W))))
  val ResultReg = RegInit(VecInit.fill(p)(VecInit.fill(q)(0.U(config.outputWidth.W))))

  val dataValid = io.in_a.valid && io.in_b.valid

  val busy = RegInit(false.B)
  //debugLog(p"${config.inputWidth}\n", LogLevel.DEBUG)
  io.in_a.ready := !busy
  io.in_b.ready := !busy
  val sysmm = Module(new SystolicMM(n, gemmType))
  sysmm.io.reset := false.B
  for (i <- 0 until n) {
    sysmm.io.in_a(i) := 0.U
    sysmm.io.in_b(i) := 0.U
  }

  when(dataValid) {
    for (i <- 0 until m) {
      for (j <- 0 until p) {
        matrixAReg(j)(i) := io.in_a.bits(j * m + i)
      }
      for (j <- 0 until q) {
        matrixBReg(i)(j) := io.in_b.bits(i * q + j)
      }
    }
    busy := true.B
  }

  val resValid = RegInit(false.B)
  io.out.valid := resValid
  for (i <- 0 until p) {
    for (j <- 0 until q) {
      io.out.bits(i * q + j) := ResultReg(i)(j)
    }
  }
  val rowCnt = p / n
  val colCnt = q / n
  val rowIndex = Counter(rowCnt + 1)
  val colIndex = Counter(colCnt + 1)
  when(rowIndex.value < rowCnt.U){
    val cnt = Counter(m + 2 * n)
    when(colIndex.value < colCnt.U){
      when(busy && cnt.value < (n + m).U) {
        for (i <- 0 until n) {
          val temp = cnt.value >= i.U
          val p = Mux(temp, cnt.value - i.U, 0.U)
          val row = rowIndex.value * n.U + i.U
          val col = colIndex.value * n.U + i.U
          when(temp && p < m.U) {
            //debugLog(p"A row: ${row} col: ${p} value: ${matrixAReg(row)(p)} \n", LogLevel.DEBUG)
            //debugLog(p"B row: ${p} col: ${col} value: ${matrixBReg(p)(col)} \n", LogLevel.DEBUG)
            sysmm.io.in_a(i) := matrixAReg(row)(p)
            sysmm.io.in_b(i) := matrixBReg(p)(col)
          }
        }
        debugLog(p"\n")
        cnt.inc()
      }.elsewhen(busy && cnt.value < (m + 2 * n - 1).U) {
        cnt.inc()
      }
      when(cnt.value === (m + 2 * n - 1).U) {
        cnt.reset()
        colIndex.inc()
        for (i <- 0 until n) {
          for (j <- 0 until n) {
            val row = rowIndex.value * n.U + i.U
            val col = colIndex.value * n.U + j.U  
            debugLog(p"row: ${row} col: ${col} value: ${sysmm.io.out(i * n + j)} \n", LogLevel.DEBUG)
            ResultReg(row)(col) := sysmm.io.out(i * n + j)
          }
        }
        
        sysmm.io.reset := true.B
      }
    }
    when(colIndex.value === colCnt.U) {
      cnt.reset()
      colIndex.inc()
      rowIndex.inc()
    }
  }
  when(rowIndex.value === rowCnt.U) {
    resValid := true.B
    //debugLog(p"busy: ${busy} resValid:${resValid} \n", LogLevel.DEBUG)
    when(io.out.ready) {
      rowIndex.reset()
      colIndex.reset()
      resValid := false.B
      busy := false.B
    }
  }
  //println(cf"row is ${rowIndex} col is ${colIndex}")
  //printf(p"${rowIndex.value === rowCnt.U}\n")
  //debugLog(p"busy: ${busy} row: ${rowIndex.value} col: ${colIndex.value} out_valid: ${io.out.valid}\n", LogLevel.DEBUG)
}

class CustomGemmQueue(val n: Int = 4, val p: Int = 512, val q: Int = 512, val m: Int = 128, val gemmType: GEMMDataType.Type, val bufferSize: Int = 32)(implicit config: DataWidthConfig)
    extends Module
    with GEMMAccuracyConfig
    with DebugLog{
  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(p * m, UInt(config.inputWidth.W))))
    val in_b = Flipped(Decoupled(Vec(m * q, UInt(config.inputWidth.W))))
    val reset = Input(Bool())
    val flush = Input(Bool())
    val gemm_out = Decoupled(Vec(p * q, UInt(config.outputWidth.W)))
    val out = Decoupled(Vec(p * q, UInt(config.outputWidth.W)))
    val done = Output(Bool())
  })

  val curBuffer = Module(
    new Queue(
      Vec(p * q, UInt(config.outputWidth.W)),
      entries = bufferSize,
      pipe = true,
      flow = false,
      useSyncReadMem = false,
      hasFlush = true
    )
  )
  val doneReg = RegInit(false.B)
  val gemm = Module(new CustomGemm(n, p, q, m, gemmType))
  gemm.io.in_a <> io.in_a
  gemm.io.in_b <> io.in_b
  gemm.io.reset <> io.reset
  curBuffer.io.flush.get := io.flush
  curBuffer.io.enq <> gemm.io.out
  doneReg := gemm.io.out.valid
  io.out <> curBuffer.io.deq
  io.gemm_out <> gemm.io.out
  io.done := doneReg
}
