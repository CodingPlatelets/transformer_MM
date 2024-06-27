package pe.utils

import chisel3._
class PipeValue[T <: Data](elements: T, val dim: Int, val numOfMask: Int) extends Bundle {
  val value = Vec(dim, elements)
  val mask = Vec(numOfMask, UInt(common.maskType.W))
}
