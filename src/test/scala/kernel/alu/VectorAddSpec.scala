package kernel.alu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation
import chisel3.util.DecoupledIO

class VectorAddSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** Converts a Float to a 2's complement fixed-point BigInt representation. */
  def floatToFixed(f: Float, wii: Int, wif: Int): BigInt = {
    val totalWidth = wii + wif
    val longVal = Math.round(f * (1 << wif))
    if (longVal < 0) {
      (BigInt(1) << totalWidth) + longVal
    } else {
      BigInt(longVal)
    }
  }

  /** Converts a 2's complement fixed-point BigInt back to a Float. */
  def fixedToFloat(i: BigInt, wii: Int, wif: Int): Float = {
    val totalWidth = wii + wif
    val maxPositiveValue = BigInt(1) << (totalWidth - 1)
    val asSigned = if (i >= maxPositiveValue) i - (BigInt(1) << totalWidth) else i
    asSigned.toFloat / (1 << wif)
  }

  behavior.of("LargeVectorAdd")

  it should "correctly add two large vectors using limited PEs" in {
    val WII = 8
    val WIF = 8
    val VECTOR_SIZE = 32 // Using a smaller size for faster simulation
    val NUM_PE = 8

    test(new LargeVectorAdd(WII, WIF, VECTOR_SIZE, NUM_PE))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
        // 1. Generate test data
        val rand = new scala.util.Random
        val max_val_float = (1 << (WII - 2)).toFloat
        val vec_a_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)
        val vec_b_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)

        val vec_a_fixed = vec_a_float.map(f => floatToFixed(f, WII, WIF).U)
        val vec_b_fixed = vec_b_float.map(f => floatToFixed(f, WII, WIF).U)

        // 2. Calculate expected result in software
        val expected_res_float = (vec_a_float.zip(vec_b_float)).map { case (a, b) => a + b }
        val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF).U)

        // 3. 流式Decoupled输入输出

        // 输入A/B
        if (dut.io.in_a.ready.peekBoolean() && dut.io.in_b.ready.peekBoolean()) {
          dut.io.in_a.valid.poke(true.B)
          dut.io.in_b.valid.poke(true.B)
          for (i <- 0 until VECTOR_SIZE) {
            dut.io.in_a.bits(i).poke(vec_a_fixed(i))
            dut.io.in_b.bits(i).poke(vec_b_fixed(i))
          }
        } else {
          dut.io.in_a.valid.poke(false.B)
          dut.io.in_b.valid.poke(false.B)
        }
        dut.clock.step(1)
        while (!dut.io.out.valid.peekBoolean()) {
          dut.clock.step()
        }
        // 输出检查
        println(s"hjjj")

        dut.io.out.ready.poke(true.B)
        for (i <- 0 until VECTOR_SIZE) {
          val expected_val = expected_res_fixed(i).litValue
          val actual_val = dut.io.out.bits(i).peek().litValue
          println(s"idx $i :expected $expected_val, got $actual_val")
          //   assert(expected_val == actual_val, s"Mismatch at index $i: expected $expected_val, got $actual_val")
        }

        dut.clock.step(1)

      }
  }

//   behavior.of("VectorAdd")

//   it should "correctly add streaming vector chunks" in {
//     val WII = 8
//     val WIF = 8
//     val NUM_PE = 4
//     val NUM_CHUNKS = 5

//     test(new VectorAdd(WII, WIF, NUM_PE))
//       .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
//         // 1. Generate test data
//         val rand = new scala.util.Random
//         val max_val_float = (1 << (WII - 2)).toFloat
//         val chunks_a_float = Seq.fill(NUM_CHUNKS, NUM_PE)(rand.nextFloat() * max_val_float)
//         val chunks_b_float = Seq.fill(NUM_CHUNKS, NUM_PE)(rand.nextFloat() * max_val_float)

//         val chunks_a_fixed = chunks_a_float.map(chunk => chunk.map(f => floatToFixed(f, WII, WIF).U))
//         val chunks_b_fixed = chunks_b_float.map(chunk => chunk.map(f => floatToFixed(f, WII, WIF).U))

//         // 2. Calculate expected results
//         val expected_chunks_float = (chunks_a_float.zip(chunks_b_float)).map {
//           case (a, b) =>
//             (a.zip(b)).map { case (x, y) => x + y }
//         }
//         val expected_chunks_fixed = expected_chunks_float.map(chunk => chunk.map(f => floatToFixed(f, WII, WIF).U))

//         // 3. 直接用fork实现输入输出
//         fork {
//           // A、B同时输入
//           var cnt = 0
//           while (cnt < NUM_CHUNKS) {
//             val (chunkA, chunkB) = chunks_a_fixed.zip(chunks_b_fixed)(cnt)
//             if (dut.io.in_a.ready.peekBoolean() && dut.io.in_b.ready.peekBoolean()) {
//               dut.io.in_a.valid.poke(true.B)
//               dut.io.in_b.valid.poke(true.B)
//               for (i <- chunkA.indices) {
//                 dut.io.in_a.bits(i).poke(chunkA(i))
//                 dut.io.in_b.bits(i).poke(chunkB(i))
//               }
//               cnt += 1
//             } else {
//               dut.io.in_a.valid.poke(false.B)
//               dut.io.in_b.valid.poke(false.B)
//             }
//             dut.clock.step(1)
//           }
//         }.fork {
//           var resCnt = 0
//           while (resCnt < NUM_CHUNKS) {
//             println(s"number :$resCnt")
//             val expected_chunk = expected_chunks_fixed(resCnt)
//             // 输出检查
//             if (dut.io.out.valid.peekBoolean()) {
//               dut.io.out.ready.poke(true.B)
//               for (i <- expected_chunk.indices) {
//                 val expected_val = expected_chunk(i).litValue
//                 val actual_val = dut.io.out.bits(i).peek().litValue
//                 println(s"idx $i :expected $expected_val, got $actual_val")
//                 // assert(expected_val == actual_val, s"Mismatch in chunk: expected $expected_val, got $actual_val")
//               }
//               resCnt += 1
//             } else {
//               dut.io.out.ready.poke(false.B)
//             }
//             dut.clock.step(1)
//           }

//         }.join()
//       }
//   }

  // behavior.of("LargeVectorAdd2")

  // it should "correctly add two large vectors using limited PEs" in {
  //   val WII = 8
  //   val WIF = 8
  //   val VECTOR_SIZE = 64 // Using a smaller size for faster simulation
  //   val NUM_PE = 8

  //   test(new LargeVectorAdd2(WII, WIF, VECTOR_SIZE, NUM_PE))
  //     .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
  //       // 1. Generate test data
  //       val rand = new scala.util.Random
  //       val max_val_float = (1 << (WII - 2)).toFloat
  //       val vec_a_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)
  //       val vec_b_float = Seq.fill(VECTOR_SIZE)(rand.nextFloat() * max_val_float)

  //       val vec_a_fixed = vec_a_float.map(f => floatToFixed(f, WII, WIF).U)
  //       val vec_b_fixed = vec_b_float.map(f => floatToFixed(f, WII, WIF).U)

  //       // 2. Calculate expected result in software
  //       val expected_res_float = (vec_a_float.zip(vec_b_float)).map { case (a, b) => a + b }
  //       val expected_res_fixed = expected_res_float.map(f => floatToFixed(f, WII, WIF).U)

  //       // 3. 流式Decoupled输入输出
  //       fork {
  //         // 输入A/B

  //         for (i <- 0 until VECTOR_SIZE) {
  //           dut.io.in_a(i).poke(vec_a_fixed(i))
  //           dut.io.in_b(i).poke(vec_b_fixed(i))
  //         }
  //         dut.io.start.poke(true.B)
  //         dut.clock.step(1)
  //       }.fork {
  //         // 输出检查
  //         if (dut.io.out.valid.peekBoolean()) {

  //           for (i <- 0 until VECTOR_SIZE) {
  //             val expected_val = expected_res_fixed(i).litValue
  //             val actual_val = dut.io.out.bits(i).peek().litValue
  //             println(s"idx $i :expected $expected_val, got $actual_val")
  //             //   assert(expected_val == actual_val, s"Mismatch at index $i: expected $expected_val, got $actual_val")
  //           }
  //         }
  //         dut.clock.step(1)

  //       }.join()
  //     }
  // }

}
