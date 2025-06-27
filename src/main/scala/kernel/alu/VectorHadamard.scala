package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog

/**
  * Performs Hadamard (element-wise) multiplication on large vectors using a limited number of parallel PEs.
  * This module acts as a controller that chunks large input vectors, feeds them
  * sequentially to a parallel PE array (`HadamardPERow`), and assembles the results.
  *
  * @param WII         Integer width of fixed-point numbers.
  * @param WIF         Fractional width of fixed-point numbers.
  * @param VECTOR_SIZE The total size of the input and output vectors.
  * @param NUM_PE      The number of parallel Processing Elements to use for computation.
  */
class HadamardPERow(
  val WII:    Int = 8,
  val WIF:    Int = 8,
  val NUM_PE: Int = 16)
    extends Module
    with DebugLog {

  val io = IO(new Bundle {
    val in_a = Flipped(Decoupled(Vec(NUM_PE, UInt((WII + WIF).W))))
    val in_b = Flipped(Decoupled(Vec(NUM_PE, UInt((WII + WIF).W))))
    val out = Decoupled(Vec(NUM_PE, UInt((WII + WIF).W)))
    val overflow = Output(Valid(Vec(NUM_PE, Bool())))
  })

  val pes = Seq.fill(NUM_PE)(Module(new FxpMul(WII, WIF, WII, WIF, WII, WIF)))

  io.in_a.ready := true.B
  io.in_b.ready := true.B

  val fire = io.in_a.valid && io.in_b.valid

  val out_bits = Wire(Vec(NUM_PE, UInt((WII + WIF).W)))
  val overflow_bits = Wire(Vec(NUM_PE, Bool()))

  for (i <- 0 until NUM_PE) {
    pes(i).io.ina.valid := fire
    pes(i).io.ina.bits := io.in_a.bits(i)
    pes(i).io.inb.valid := fire
    pes(i).io.inb.bits := io.in_b.bits(i)
    out_bits(i) := pes(i).io.out.bits
    overflow_bits(i) := pes(i).io.overflow.bits
  }

  val out_valid = pes(0).io.out.valid
  io.out.valid := out_valid
  io.out.bits := out_bits
  io.overflow.valid := out_valid
  io.overflow.bits := overflow_bits
}

class VectorHadamard(
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
  io.in_a.ready := true.B
  io.in_b.ready := true.B
  io.out.valid := false.B
  io.out.bits := DontCare

  val hadamardPERow = Module(new HadamardPERow(WII, WIF, NUM_PE))
  val chunk_counter = Counter(NUM_CHUNKS)
  val in_a_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val in_b_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val out_buf = Reg(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  val out_valid = RegInit(false.B)

  object state extends ChiselEnum {
    val idle, input, compute, done = Value
  }
  val stateReg = RegInit(state.idle)

  hadamardPERow.io.in_a.valid := false.B
  hadamardPERow.io.in_b.valid := false.B
  hadamardPERow.io.out.ready := false.B
  hadamardPERow.io.in_a.bits := VecInit.fill(NUM_PE)(0.U)
  hadamardPERow.io.in_b.bits := VecInit.fill(NUM_PE)(0.U)

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
      hadamardPERow.io.in_a.valid := true.B
      hadamardPERow.io.in_b.valid := true.B
      for (i <- 0 until NUM_PE) {
        hadamardPERow.io.in_a.bits(i) := in_a_buf(chunk_counter.value * NUM_PE.U + i.U)
        hadamardPERow.io.in_b.bits(i) := in_b_buf(chunk_counter.value * NUM_PE.U + i.U)
      }
      stateReg := state.compute
    }
    is(state.compute) {
      hadamardPERow.io.out.ready := true.B
      when(hadamardPERow.io.out.valid) {

        // for (c <- 0 until NUM_CHUNKS) {
        //   when(chunk_counter.value === c.U) {
        //     val base_addr = c * NUM_PE
        //     for (i <- 0 until NUM_PE) {
        //       out_buf(base_addr + i) := hadamardPERow.io.out.bits(i)
        //     }
        //   }
        // }
        for (i <- 0 until NUM_PE) {
          out_buf(chunk_counter.value * NUM_PE.U + i.U) := hadamardPERow.io.out.bits(i)
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
      io.in_a.ready := true.B
      io.in_b.ready := true.B
      io.out.valid := out_valid
      io.out.bits := out_buf
      stateReg := state.idle
    }
  }
} 