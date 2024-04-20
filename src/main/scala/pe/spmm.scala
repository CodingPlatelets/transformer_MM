package pe

import chisel3._
import chisel3.util._

class spmm() {

}

class NumDotVec(val dim: Int) extends Module {
  val io = IO(new Bundle {
    val num = Input(UInt(8.W))
    val vec = Input(Vec(dim, UInt(8.W)))
    val res = Output(Vec(dim, UInt(16.W)))
  })
  val temp = VecInit(Seq.fill(dim)(0.U(16.W)))
  for (i <- 0 until dim) {
    temp(i) := RegNext(io.num * io.vec(i))
  }
  io.res := temp
}


// TODO: the seq will change in every terms of the pipeline. How to design a fit seq to contain the numbers?
class SeqDotVecs(val seq: Int, val dim: Int) extends Module {
  val io = IO(new Bundle {
    val spareNums = Input(Vec(seq, UInt(8.W)))
    val maskVec = Input(Vec(dim, Bool()))
    val vecs = Input(Vec(dim, Vec(dim, UInt(8.W))))
    val res = Output(Vec(dim, UInt(16.W)))
  })

  val temp = VecInit(Seq.fill(dim)(RegInit(0.U(16.W))))
  val maskVecWithIndex = io.maskVec.zipWithIndex.collect { case (bool, index) if bool.litToBooleanOption.getOrElse(false) => index }
  val counter = Counter(seq)

  for (i <- maskVecWithIndex.indices) {
    val numDotVecModule = Module(new NumDotVec(dim))
    numDotVecModule.io.num := io.spareNums(i)
    numDotVecModule.io.vec := io.vecs(maskVecWithIndex(i))
    temp := temp.zip(numDotVecModule.io.res).map { case (a, b) => RegNext(a +& b) }
  }

  io.res := temp
}

