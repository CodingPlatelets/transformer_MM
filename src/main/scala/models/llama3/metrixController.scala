package models.llama3

import common.llamaConfig
import chisel3._
import chisel3.util._
import kernel.alu.GEMM
import kernel.utils.ForwardingMemory
class metrixController extends Module with llamaConfig {}

/*
 * matrix mul matrix
 * matrixA is [inputN, dim]
 * matrixB is [dim, head_dim]
 * matrixC is [inputN, head_dim]
 */
class QKVGenerationMul extends Module with llamaConfig {
  val io = IO(new Bundle {
    val matrixAPart = Input(Vec(minN, Vec(dim, UInt(bits.W))))
  })
}

class SystolicGroup extends Module with llamaConfig {
  val io = IO(new Bundle {
    val matrixAVec = Flipped(Decoupled(Vec(systolicGroupSize, Vec(systolicSize, Vec(systolicSize, UInt(bits.W))))))
    val matrixBVec = Flipped(Decoupled(Vec(systolicGroupSize, Vec(systolicSize, Vec(systolicSize, UInt(bits.W))))))
    val matrixCVec = Decoupled(Vec(systolicGroupSize, Vec(systolicSize * systolicSize, UInt(bits.W))))
  })

  val gemmRow = for (i <- 0 until systolicGroupSize) yield Module(new GEMM(16))

  val matrixAValid = io.matrixAVec.valid
  val matrixBValid = io.matrixBVec.valid

  io.matrixAVec.ready := gemmRow.map(_.InputA.ready).reduce(_ && _)
  io.matrixBVec.ready := gemmRow.map(_.InputB.ready).reduce(_ && _)

  val matrixCValid = gemmRow.map(_.OutputPipe.valid).reduce(_ && _)

  for (i <- 0 until systolicGroupSize) {
    gemmRow(i).InputA.bits := io.matrixAVec.bits(i)
    gemmRow(i).InputA.valid := matrixAValid
    gemmRow(i).InputB.bits := io.matrixBVec.bits(i)
    gemmRow(i).InputB.valid := matrixBValid
    gemmRow(i).accMode := false.B
    io.matrixCVec.bits(i) := gemmRow(i).OutputPipe.bits
    gemmRow(i).OutputPipe.ready := io.matrixCVec.ready
  }

  io.matrixCVec.valid := matrixCValid

}

class GEMMController(val x: Int, val y: Int) extends Module with llamaConfig {
  assert(x % (systolicGroupSize * systolicSize) == 0 && y % systolicSize == 0)
}
