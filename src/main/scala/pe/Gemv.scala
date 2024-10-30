package pe

import chisel3._
import pe.utils.FxpAdd
import pe.utils.FxpMul
import pe.utils.common

class VecDotVec(val size: Int = 4, val bits: Int = 8) extends Module {
  val io = IO(new Bundle {
    val vecA = Input(Vec(size, UInt(bits.W)))
    val vecB = Input(Vec(size, UInt(bits.W)))
    val res = Output(UInt((bits * 2 + 1).W))
  })

  val sumMultiply = VecInit((io.vecA.zip(io.vecB)).map { case (a, b) => a * b })
  io.res := sumMultiply.reduceTree((a, b) => RegNext(a +& b))
}

// class VecDotVecFxp(val size: Int = 4, val WII: Int, val WIF: Int, val WOI: Int, val WOF: Int) extends Module {
//   val vecA = IO(Input(Vec(size, UInt((WII + WIF).W))))
//   val vecB = IO(Input(Vec(size, UInt((WII + WIF).W))))

//   val res = IO(Output(UInt((WOI + WOF).W)))

//   val FxpMulVec = VecInit(
//     (vecA.zip(vecB)).map {
//       case (a, b) => {
//         val mul = Module(new FxpMul(WII, WIF, WII, WIF, WII, WIF))
//         mul.io.ina <> a
//         mul.io.inb <> b
//         mul.io.out
//       }
//     }
//   )

//   res := FxpMulVec.reduceTree((a, b) => {
//     val add = Module(new FxpAdd(WII, WIF, WII, WIF, WOI, WOF))
//     add.io.ina.bits <> a
//     add.io.inb.bits <> b
//     add.io.overflow := DontCare
//     add.io.out.bits
//   })

// }
