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
class VectorAdd(
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