package pe

import chisel3._
import chisel3.util._
import configs.SdpmmConfigs
import vitiskernel.util.DebugLog
import fixedpoint._
import pe.utils.PipeValue

class Softmax extends Module with DebugLog {

  val InputPipe = IO(
    Flipped(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))
  )
  val OutputPipe = IO(Decoupled(new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask)))

  val InputQueue = Module(
    new Queue(
      new PipeValue(UInt(SdpmmConfigs.bit.W), SdpmmConfigs.dim, SdpmmConfigs.numOfMask),
      SdpmmConfigs.queueSize,
      pipe = false,
      flow = false,
      useSyncReadMem = false
    )
  )

}

class CORDICExp(val bit: Int, val iter: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(bit.W))
    val exp_x = Output(SInt(bit.W))
  })

  // CORDIC constants for exponential calculation
  // val K = (1.64676025812107).S(32.W) // Approximate CORDIC gain for exp
  val K = (1.64676025812107 * (1 << 16)).toInt.S(bit.W) // Convert to fixed-point and then to SInt
  val one = (1 << 20).S(bit.W) // 1 represented in fixed point, adjust as needed

  // Registers initialization
  val x = RegInit(io.x)
  val y = RegInit(one) // Start with 1.0 in fixed point
  val z = RegInit(0.S(bit.W))
  val counter = RegInit(0.U(log2Ceil(iter).W))

  // CORDIC iteration for exponential calculation
  when(counter < iter.U) {
    val d = Mux(z < 0.S, -1.S, 1.S)
    x := x // x remains unchanged in exponential CORDIC
    y := y + (d * y) >> counter // Update y based on z's sign
    z := z - (d * (K >> counter)) // Update z based on iteration

    counter := counter + 1.U
  }

  // Output the result
  io.exp_x := y // The exponential result is in y

  // Debugging information
  printf(p"x: ${x}, y: ${y}, z: ${z}, counter: ${counter}\n")
}

class FixedPointExp(val wholeWidth: Int, val fractionalWidth: Int) extends Module with DebugLog {
  val io = IO(new Bundle {
    val x = Input(SInt((wholeWidth).W))
    val exp_x = Output(SInt((wholeWidth).W))
  })

  // z = floor(-x/log2)
  val z = Wire(SInt(((wholeWidth).W)))
  val p = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val lp = Wire(FixedPoint((wholeWidth).W, fractionalWidth.BP))
  val log2 = WireDefault(0.6931471805599453.F(fractionalWidth.BP))
  val bias1 = WireDefault(1.353.F(fractionalWidth.BP))
  val k1 = WireDefault(0.3585.F(fractionalWidth.BP))
  val bias2 = WireDefault(0.344.F(fractionalWidth.BP))

  z := io.x / log2.asSInt
  p := io.x.asFixedPoint(fractionalWidth.BP) + z.asFixedPoint(fractionalWidth.BP) * log2
  lp := k1 * (p + bias1) * (p + bias1) + bias2
  io.exp_x := (lp >> z.asUInt).asSInt
}
