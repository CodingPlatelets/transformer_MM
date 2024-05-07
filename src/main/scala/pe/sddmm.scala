package pe

import chisel3._
import chisel3.util.Decoupled

class VecDotVec(val dim: Int) extends Module {

  val io = IO(new Bundle {
    val rowQ = Input(Vec(dim, UInt(8.W)))
    val rowK = Input(Vec(dim, UInt(8.W)))
    val res = Output(UInt((16 + dim).W))
  })

  // multiply each element of rowQ with each element of rowK and sum them using Vec.reduceTree function
  val sumMultiply = VecInit((io.rowQ.zip(io.rowK)).map { case (a, b) => a * b })
  io.res := sumMultiply.reduceTree((a, b) => RegNext(a +& b))

}

// todo: cannot create pes dynamically, so we must fix the numOfMask, but we can set a min numOfMask and schedule it in the future
class sddmm(bit: Int = 8, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4) extends Module {
  val io = IO(new Bundle {
    val mask = Input(Vec(L, Vec(L, Bool())))
    val qVec = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val qIndex = Input(UInt(D.W))
    val kMatrix = Input(Vec(D, Vec(L, UInt(bit.W))))
  })

  val pes = for {
    i <- 0 until L
    j <- 0 until numOfMask
  } yield Module(new PE((2 * bit), (i, j), 0))

  for (i <- 0 until numOfMask) {
    pes(i).io := DontCare
  }

  val tempMaskReg = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(L)(false.B)))))
}
