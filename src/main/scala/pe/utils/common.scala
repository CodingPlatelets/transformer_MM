package pe.utils

import chisel3._

import chisel3.util._
object common {
  def counter(max: UInt, cond: Bool) = {
    val x = RegInit(0.U(max.getWidth.W))
    when(cond) {
      x := Mux(x === max, 0.U, x + 1.U)
    }
    x
  }

  def counter(max: Int, initCond: Bool) = {
    val x = RegInit(0.U(log2Ceil(max).W))
    when(initCond && x === 0.U) {
      x := x + 1.U
    }.elsewhen(x =/= 0.U) {
      x := Mux(x === max.U, 0.U, x + 1.U)
    }
    x
  }

  // this will find the last "one" in an UInt, and then convert it to a one hot num
  def maskOH(mask: UInt) = {
    mask - (mask & (mask - 1.U))
  }

  val maskType = 16
  val DATA_WIDTH = 512
  val PIPE_DATA_WIDTH = 1024
  val valueType = UInt(16.W)

  object AddOrSub extends ChiselEnum {
    val ADD, SUB = Value
  }
}
