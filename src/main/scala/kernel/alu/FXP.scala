package kernel.alu

import chisel3._

class FXP(val IntWidth: Int, val FractionalWidth: Int) extends Bundle {
  def signWidth = IntWidth + FractionalWidth - 1
}

object FXP {
  def hex2SignedInt(hex: String, width: Int) = {
    val maxUnsignedValue = BigInt(2).pow(width)
    val maxPositiveValue = BigInt(2).pow(width - 1)
    val resTmp = BigInt(hex, 16)
    if (resTmp >= maxPositiveValue) { resTmp - maxUnsignedValue }
    else { resTmp }
  }

  def maxUnsignedValue(IntWidth: Int, FractionalWidth: Int) = BigInt(2).pow(IntWidth + FractionalWidth)
  def maxPositiveValue(IntWidth: Int, FractionalWidth: Int) = BigInt(2).pow(IntWidth + FractionalWidth - 1)

  def bits2RealValue(value: BigInt, IntWidth: Int, FractionalWidth: Int) = {
    val maxPositiveValue = FXP.maxPositiveValue(IntWidth, FractionalWidth)
    BigDecimal(if (value >= maxPositiveValue) { value - maxUnsignedValue(IntWidth, FractionalWidth) }
    else { value }) / (BigDecimal(2).pow(FractionalWidth))
  }

}
