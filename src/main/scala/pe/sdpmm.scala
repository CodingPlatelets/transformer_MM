package pe

import chisel3._
import chisel3.util._

class sdpmm(val bit: Int = 16, D: Int = 32, val L: Int = 32, val numOfMask: Int = 4, val queueSize: Int = 10)
    extends Module {
  val io = IO(new Bundle {
    val inMask = Flipped(Decoupled(Vec(numOfMask, UInt(utils.maskType.W))))
    val nums = Flipped(Decoupled(Vec(D, UInt(bit.W))))
    val res = Decoupled(Vec(D, UInt(bit.W)))
    val outMask = Decoupled(Vec(numOfMask, UInt(utils.maskType.W)))

    val kMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
    val vMatrix = Input(Vec(L, Vec(D, UInt(bit.W))))
  })

//   val temV = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
//   temV := io.vMatrix
//   val tempK = RegInit(VecInit(Seq.fill(L)(VecInit(Seq.fill(D)(0.U(bit.W))))))
//   tempK := io.kMatrix

  val sddmm = Module(new sddmm(bit, D, L, numOfMask, queueSize))
  val SpMM = Module(new SpMM(bit, D, L, 1, numOfMask, queueSize))

  sddmm.io.kMatrix := io.kMatrix
  SpMM.io.vMatrix := io.vMatrix

  sddmm.io.inMask <> io.inMask
  sddmm.io.qVec <> io.nums

  // todo: softmax can be added here
  SpMM.io.inMask <> sddmm.io.outMask
  SpMM.io.nums <> sddmm.io.res

  io.res <> SpMM.io.res
  io.outMask <> SpMM.io.outMask

}
