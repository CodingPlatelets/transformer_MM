package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog

/**
 * Performs vector addition on streaming data chunks.
 * It takes two vectors `in_a` and `in_b` chunk by chunk, adds them element-wise,
 * and streams out the resulting vector chunk. This is done in parallel using a
 * configurable number of Processing Elements (PEs).
 *
 * @param WII     Integer width of fixed-point numbers.
 * @param WIF     Fractional width of fixed-point numbers.
 * @param NUM_PE  Number of parallel Processing Elements, which also defines the chunk size.
 */
class AddPERow(
    val WII: Int = 8,
    val WIF: Int = 8,
    val NUM_PE: Int = 16
) extends Module
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(NUM_PE, UInt((WII + WIF).W))))
    val in_b = Flipped(Decoupled(Vec(NUM_PE, UInt((WII + WIF).W))))
    val out = Decoupled(Vec(NUM_PE, UInt((WII + WIF).W)))

    /** A vector of booleans indicating an overflow for each element-wise addition.
      * This signal is valid when `out` is valid.
      */
    val overflow = Output(Valid(Vec(NUM_PE, Bool())))
  })

  // Instantiate NUM_PE parallel Processing Elements (PEs) using FxpAdd.
  // The output width of the adder is kept the same as the input width.
  // FxpAdd handles potential overflow and rounding internally via FxpZoom.
  val pes = Seq.fill(NUM_PE)(Module(new FxpAdd(WII, WIF, WII, WIF, WII, WIF)))

  // --- Handshake Logic ---

  // We are ready to accept input chunks if the downstream consumer is ready for our output.
  io.in_a.ready := true.B
  io.in_b.ready := true.B
  

  // A transaction (a chunk computation) fires when both inputs are valid and we are ready.
  val fire = io.in_a.valid && io.in_b.valid


  // --- PE Connections ---

  val out_bits = Wire(Vec(NUM_PE, UInt((WII + WIF).W)))
  val overflow_bits = Wire(Vec(NUM_PE, Bool()))

  for (i <- 0 until NUM_PE) {
    // Feed data to each PE only when a 'fire' event occurs.
    pes(i).io.ina.valid := fire
    pes(i).io.ina.bits := io.in_a.bits(i)

    pes(i).io.inb.valid := fire
    pes(i).io.inb.bits := io.in_b.bits(i)

    // Collect results and overflow flags from each PE.
    out_bits(i) := pes(i).io.out.bits
    overflow_bits(i) := pes(i).io.overflow.bits
  }

  // --- Output Logic ---

  // The output is valid when the PEs signal that their computation is done.
  // All PEs have the same latency, so we can just use the valid signal from the first one.
  val out_valid = pes(0).io.out.valid
  io.out.valid := out_valid
  io.out.bits := out_bits

  io.overflow.valid := out_valid
  io.overflow.bits := overflow_bits
} 


/**
  * Performs vector addition on large vectors using a limited number of parallel PEs.
  * This module acts as a controller that chunks large input vectors, feeds them
  * sequentially to a parallel PE array (`VectorAdd`), and assembles the results.
  *
  * @param WII         Integer width of fixed-point numbers.
  * @param WIF         Fractional width of fixed-point numbers.
  * @param VECTOR_SIZE The total size of the input and output vectors.
  * @param NUM_PE      The number of parallel Processing Elements to use for computation.
  */
class VectorAdd(
  val WII:         Int = 8,
  val WIF:         Int = 8,
  val VECTOR_SIZE: Int = 128,
  val NUM_PE:      Int = 8)
    extends Module
    with DebugLog {

  require(VECTOR_SIZE > 0, "Vector size must be positive.")
  require(NUM_PE > 0, "Number of PEs must be positive.")
  require(VECTOR_SIZE % NUM_PE == 0, "VECTOR_SIZE must be a multiple of NUM_PE.")

  val NUM_CHUNKS = VECTOR_SIZE / NUM_PE

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val in_b = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val out = Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  })

  val dataValid = io.in_a.valid && io.in_b.valid
  // 默认输出
  io.in_a.ready := true.B
  io.in_b.ready := true.B
  io.out.valid := false.B
  io.out.bits := DontCare

  // 内部PE阵列
  val vectorAddPEs = Module(new AddPERow(WII, WIF, NUM_PE))

  // chunk计数器
  val chunk_counter = Counter(NUM_CHUNKS)

  // 输入缓存
  val in_a_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val in_b_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))

  // 输出缓存
  val out_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val out_valid = RegInit(false.B)

  // 状态机
  object state extends ChiselEnum {
    val idle, input, compute, done = Value
  }
  val stateReg = RegInit(state.idle)

  // VectorAdd接口
  vectorAddPEs.io.in_a.valid := false.B
  vectorAddPEs.io.in_b.valid := false.B
  vectorAddPEs.io.out.ready := false.B
  vectorAddPEs.io.in_a.bits := VecInit.fill(NUM_PE)(0.U)
  vectorAddPEs.io.in_b.bits := VecInit.fill(NUM_PE)(0.U)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        io.in_a.ready := false.B
        io.in_b.ready := false.B
        in_a_buf := io.in_a.bits
        in_b_buf := io.in_b.bits
        chunk_counter.value := 0.U
        stateReg := state.input
      }
    }
    is(state.input) {
      vectorAddPEs.io.in_a.valid := true.B
      vectorAddPEs.io.in_b.valid := true.B
      // val base = chunk_counter.value * NUM_PE.U
      // printf(p"basssssss $base\n\n")

      for (i <- 0 until NUM_PE) {
        vectorAddPEs.io.in_a.bits(i) := in_a_buf(chunk_counter.value * NUM_PE.U + i.U)
        vectorAddPEs.io.in_b.bits(i) := in_b_buf(chunk_counter.value * NUM_PE.U + i.U)
        // printf(p"$base ,i : $i ,aaaaaa: ${in_a_buf(chunk_counter.value * NUM_PE.U + i.U)}\n ")
      }
      stateReg := state.compute
    }
    is(state.compute) {
      // 取当前chunk
      // val base = chunk_counter.value * NUM_PE.U
      vectorAddPEs.io.out.ready := true.B
      when(vectorAddPEs.io.out.valid) {
        for (i <- 0 until NUM_PE) {
          out_buf(chunk_counter.value * NUM_PE.U + i.U) := vectorAddPEs.io.out.bits(i)
          // printf(p"$base ,i : $i ,bits: ${vectorAddPEs.io.out.bits(i)}\n ")
        }
        chunk_counter.inc()
        when(chunk_counter.value === (NUM_CHUNKS - 1).U) {
          out_valid := true.B
          stateReg := state.done
        }.otherwise {
          stateReg := state.input
        }
      }
    }
    is(state.done) {
      printf(p"jsdj\n")
      io.in_a.ready := true.B
      io.in_b.ready := true.B
      io.out.valid := out_valid
      io.out.bits := out_buf
      stateReg := state.idle

    }
  }
}