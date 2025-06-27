package kernel.alu

import chisel3._
import chisel3.util._
import kernel.utils.DebugLog
import firrtl2.Utils.True

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
class ResADD(
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
    val x = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val residual = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val addRes = Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  })

  val addOpt = Module(new VectorAdd(WII, WIF, VECTOR_SIZE, NUM_PE))

  addOpt.io.in_a <> io.x
  addOpt.io.in_b <> io.residual
  io.addRes <> addOpt.io.out

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
class LayerNorm(
  val WII:         Int = 8,
  val WIF:         Int = 8,
  val VECTOR_SIZE: Int = 128,
  val NUM_PE:      Int = 8)
    extends Module
    with DebugLog {

  val io = IO(new Bundle {
    val x = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val weight = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val bias = Flipped(Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W))))
    val normRes = Decoupled(Vec(VECTOR_SIZE, UInt((WII + WIF).W)))
  })

  val normOpt = Module(new NormalizedModule(WII, WIF, WII, WIF, VECTOR_SIZE))
  val addOpt = Module(new VectorAdd(WII, WIF, VECTOR_SIZE, NUM_PE))
  val hadamardOpt = Module(new VectorHadamard(WII, WIF, VECTOR_SIZE, NUM_PE))

  normOpt.io.in <> io.x

  hadamardOpt.io.in_a.bits := normOpt.io.out.bits
  hadamardOpt.io.in_a.valid := normOpt.io.out.valid
  hadamardOpt.io.in_b <> io.weight

  addOpt.io.in_a <> hadamardOpt.io.out
  addOpt.io.in_b <> io.bias
  io.normRes <> addOpt.io.out

}
