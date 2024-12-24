package models.llama3.common

trait llamaConfig {
  val dim = 4096
  val n_layers = 32
  val n_heads: Int = 32

  val m = 16
  val p = 8
  val q = 24

  // head_dim is the dimension of each head
  val head_dim: Int = dim / n_heads
  val maxN:     Int = 8 * 1024
  val minN:     Int = 65

  // fixed-point accuracy
  val fx_int = 4
  val fx_frac = 12

  // UInt width
  val bits = 16

  // systolic array size
  val systolicSizeGen = 4
  val systolicGroupSizeGen = 1
  val systolicSizeMul = 4
  val systolicGroupSizeMul = 1

  // DAC for zb, stream for heads
  val stream_size = 8

  // buffer size for gemm-gemm pipeline
  val bufferSizeGemm = 32

}
