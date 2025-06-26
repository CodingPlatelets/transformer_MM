package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog

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
class LargeVectorAdd(
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
  val vectorAddPEs = Module(new VectorAdd(WII, WIF, NUM_PE))

  // chunk计数器
//   val chunk_counter = RegInit(0.U(log2Ceil(NUM_CHUNKS).W))
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
  // val sIdle :: sCompute :: sOut :: Nil = Enum(3)
  // val state = RegInit(sIdle)

  // VectorAdd接口
  vectorAddPEs.io.in_a.valid := false.B
  vectorAddPEs.io.in_b.valid := false.B
  vectorAddPEs.io.out.ready := false.B
  vectorAddPEs.io.in_a.bits := VecInit.fill(NUM_PE)(0.U)
  vectorAddPEs.io.in_b.bits := VecInit.fill(NUM_PE)(0.U)

  switch(stateReg) {
    is(state.idle) {
      when(dataValid) {
        printf(p"jsdj")
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
      val base = chunk_counter.value * NUM_PE.U
      printf(p"basssssss $base\n\n")

      for (i <- 0 until NUM_PE) {
        vectorAddPEs.io.in_a.bits(i) := in_a_buf(chunk_counter.value * NUM_PE.U + i.U)
        vectorAddPEs.io.in_b.bits(i) := in_b_buf(chunk_counter.value * NUM_PE.U + i.U)
        printf(p"$base ,i : $i ,aaaaaa: ${in_a_buf(chunk_counter.value * NUM_PE.U + i.U)}\n ")
      }
      stateReg := state.compute
    }
    is(state.compute) {
      // 取当前chunk
      val base = chunk_counter.value * NUM_PE.U

      vectorAddPEs.io.out.ready := true.B
      when(vectorAddPEs.io.out.valid) {
        for (i <- 0 until NUM_PE) {
          out_buf(base + i.U) := vectorAddPEs.io.out.bits(i)
          printf(p"$base ,i : $i ,bits: ${vectorAddPEs.io.out.bits(i)}\n ")
        }
        // out_buf := next_out_reg
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

class LargeVectorAdd2(
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
    // --- Control Interface ---
    /** A pulse to start the computation for one pair of large vectors. */
    val start = Input(Bool())

    /** High while the module is processing. */
    val busy = Output(Bool())

    // --- Data Interface ---
    val in_a = Input(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
    val in_b = Input(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))

    /** The result vector is available on `out.bits` when `out.valid` is high for one cycle. */
    val out = Valid(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  })

  // --- Internal Components ---
  // The "worker" module that performs chunk-level vector addition.
  val vectorAddPEs = Module(new VectorAdd(WII, WIF, NUM_PE))

  // --- Registers for data storage and state ---
  val in_a_reg = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val in_b_reg = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val out_reg = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val out_valid_reg = RegInit(false.B)

  // --- State Machine ---
  val sIdle :: sCompute :: Nil = Enum(2)
  val state = RegInit(sIdle)

  // A counter to iterate through the chunks of the large vectors.
  val chunk_counter = Counter(NUM_CHUNKS)

  // --- Default assignments ---
  io.busy := (state =/= sIdle)
  io.out.valid := out_valid_reg
  io.out.bits := out_reg

  vectorAddPEs.io.in_a.valid := false.B
  vectorAddPEs.io.in_b.valid := false.B
  vectorAddPEs.io.out.ready := false.B
  vectorAddPEs.io.in_a.bits := VecInit.fill(NUM_PE)(0.U)
  vectorAddPEs.io.in_b.bits := VecInit.fill(NUM_PE)(0.U)

  // The output valid signal is a pulse, so it's normally false.
  out_valid_reg := false.B

  // --- State Machine Logic ---
  switch(state) {
    is(sIdle) {
      when(io.start) {
        // Latch inputs, reset counter, and start computation
        in_a_reg := io.in_a
        in_b_reg := io.in_b
        vectorAddPEs.io.in_a.valid := true.B
        vectorAddPEs.io.in_b.valid := true.B
        state := sCompute
        chunk_counter.value := 0.U
      }
    }

    is(sCompute) {
      // Feed a chunk to the PE array. We signal valid to the PEs only when we are in this state.

      val current_chunk_base_addr = chunk_counter.value * NUM_PE.U
      for (i <- 0 until NUM_PE) {
        vectorAddPEs.io.in_a.bits(i) := in_a_reg(current_chunk_base_addr + i.U)
        vectorAddPEs.io.in_b.bits(i) := in_b_reg(current_chunk_base_addr + i.U)
      }
      vectorAddPEs.io.in_a.valid := false.B
      vectorAddPEs.io.in_b.valid := false.B

      // We are always ready to accept the result from our internal PE array.
      vectorAddPEs.io.out.ready := true.B
      // When the PE array produces a valid output chunk...
      when(vectorAddPEs.io.out.valid) {
        // To avoid dynamic indexing for writes, we decode the counter
        // and use static indices. This generates a clean multiplexer.
        // for (c <- 0 until NUM_CHUNKS) {
        //   when(chunk_counter.value === c.U) {
        //     val base_addr = c * NUM_PE
        //     for (i <- 0 until NUM_PE) {
        //       out_reg(base_addr + i) := vectorAddPEs.io.out.bits(i)
        //     }
        //   }
        // }
        for (i <- 0 until NUM_PE) {
          out_reg(current_chunk_base_addr + i.U) := vectorAddPEs.io.out.bits(i)
          printf(p"$current_chunk_base_addr ,i : $i ,bits: ${vectorAddPEs.io.out.bits(i)}\n ")
        }

        // ...and move to the next chunk.
        chunk_counter.inc()
        vectorAddPEs.io.in_a.valid := true.B
        vectorAddPEs.io.in_b.valid := true.B
        // If that was the last chunk, finish the computation.
        when(chunk_counter.value === (NUM_CHUNKS - 1).U) {
          state := sIdle
          out_valid_reg := true.B // Make output valid for one cycle
        }
      }
    }
  }
}
