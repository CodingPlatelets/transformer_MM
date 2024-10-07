A hardware implementation of the general transformer-based LLM, with easily scaling and testing by chisel3.

The implementation includes Fixedpoint, IEEE float utils, GEMM, Vector dot matrix, Softmax and Exp in fixedpoint, xilinx mem reader and writer, and K-Vcache(todo).