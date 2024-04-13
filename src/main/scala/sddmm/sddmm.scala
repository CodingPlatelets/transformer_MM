package sddmm

import chisel3._

class VecMul(val dim: Int) extends Module {

  val io = IO(new Bundle {
    val rowQ = Input(Vec(dim, UInt(8.W)))
    val rowK = Input(Vec(dim, UInt(8.W)))
    val out = Output(UInt((16 + dim).W))
  })

  // multiply each element of rowQ with each element of rowK and sum them using Vec.reduceTree function
  val sumMultiply = VecInit((io.rowQ zip io.rowK).map { case (a, b) => a * b })
  io.out := sumMultiply.reduceTree((a, b) => RegNext(a +& b))

}
