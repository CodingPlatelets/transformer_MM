package kernel

import chisel3._
import chisel3.util._

// The context is a Vector of the output of the Attention Layer called VecE
// It will do in these steps:
// 1. Multiply VecE and MatrixWupper and Add Bias
// 2. ReLU the result VecE
// 3. Multiply the result VecE and MatrixWdowner and Add Bias
// 4. Add the result Vec and the previous VecE
class LinearLayer(val dimE: Int, val dimW: Int) extends Module {}

