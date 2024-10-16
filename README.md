# Accelerator for LLM Based on Chisel3

__This is a transformer-based large language model accelerator developed using Chisel HDL as the development language.__

1. **PE (Processing Element)**: A simple *MAC* (Multiply-Accumulate) unit implementation, including the necessary input and output interfaces to form a systolic array.
   
2. **ALU (Arithmetic Logic Unit)**:
   1. **Floating-point computation unit**: A single-precision floating-point computation unit following the IEEE-754 standard. It supports basic operations such as addition, subtraction, multiplication, and division. The execution cycles are as follows: 4 cycles for addition and subtraction, 2 cycles for multiplication, and 5 cycles for division. ***TODO***: The conversion module between floating-point and fixed-point numbers is still under development.
   2. **Fixed-point computation unit**: A standard fixed-point computation unit with configurable bit width and decimal point position. It supports addition, subtraction, multiplication, division, and square root operations. The execution cycles are as follows: 2 cycles for addition and subtraction, 2 cycles for multiplication, `WOI + WOF + 5` cycles for division, and `(WII + 2 - 1) / 2 + WIF + 3` cycles for square root. These cycle counts include two cycles for the number scaling module. Additionally, it supports integer division with remainder using the SRT-16 algorithm, with an execution time of 2 to `5 + (len + 3) / 2` cycles.
   3. **GEMM (General Matrix Multiply) unit**: A systolic array algorithm for matrix multiplication of `n x n` square matrices implemented with fixed-point `UInt`. It produces the result in 3n cycles.
   4. **GEMV (General Matrix-Vector Multiply) unit**: Implements a vector-matrix multiplication algorithm similar to a reduction tree, using both fixed-point `UInt` and `FXP`. The result is available in `log(n) + 2` cycles.
   5. **Softmax computation unit**: Uses a special exponential function unit based on the algorithm from [I-BERT](https://arxiv.org/abs/2101.01321) to compute exponentiation. The normalization is computed using the aforementioned `FXP` library's square root and division modules. The exact cycle count depends on the specific implementation.
   6. **Deprecated**: Matrix computation modules for SPMM (Sparse-Dense Matrix Multiplication) and SDDMM (Sampled Dense-Dense Matrix Multiplication), implemented with MAC units and mask vectors.

3. **Memory (Mem)**:
   1. **Custom SRAM module**: A customizable SRAM module where both the size and data type can be defined. Ensure there is sufficient on-chip space when using this module.
   2. **HBM/DRAM streaming read/write module**: This module allows for high-bandwidth memory (HBM) or dynamic random-access memory (DRAM) streaming, where burst length and AXI maximum bit width limitations can be ignored. The module automatically slices and transfers data according to the defined size.
   3. **AXI reader and writer interfaces**: Native AXI interfaces for reading and writing data.




__这是一个使用Chisel HDL作为开发语言的 transformer-based 大语言模型加速器__

1. PE: 简单的 *mac* 单元实现，包含构成脉动阵列必须的出口和入口。
2. ALU:
   1. 浮点计算单元，遵循 IEEE-754 标准的单精度浮点数计算单元。可以实现浮点数的加，减，乘，除，四种基本运算，耗时周期分别为：4（加减法），2（乘法），5（除法）。TODO：目前浮点数与定点数的相互转换模块实现还待完善。
   2. 定点数计算单元，标准的定点计算单元，可以自由缩放定点数位宽以及小数点位置。可以实现定点数的加，减，乘，除，开方运算，耗时周期分别为：2（加减法），2（乘法），WOI+WOF + 5（除法），（WII+2-1）/2 + WIF + 3（开方）。以上周期均包含两个周期的数字缩放模块用时。同时还拥有使用SRT-16算法得到的带余数的整数除法，运算周期为 2 ~ (5 + (len+3) / 2)
   3. GEMM 运算单元，使用定点数 UInt 实现的 n * n 正方形矩阵的脉动阵列算法，可以在 3n 个周期后得到矩阵结果。
   4. GEMV 运算单元，使用定点数UInt和定点数FXP均实现了类似于归并树的向量矩阵乘算法，可以在（logn+2）周期后得到结果。
   5. Softmax 运算单元，使用[特殊的指数运算单元](https://arxiv.org/abs/2101.01321) I-Bert中的算法计算指数函数的值，使用上述FXP库中的开方以及除法计算正规化的值，具体周期数与实际代码相关。
   6. deprcated：使用mac单元和mask向量计算的SPMM和SDDMM矩阵计算模块
3. Mem：
   1. 自定义的 SRAM 模块，可以自定义SRAM的大小和数据类型，需保证使用时片上有足够的空间
   2. 类似流式读取的 HBM/DRAM 读写模块，使用时可以忽略 burst length 和 AXI 最大位宽的限制，模块将自动切片并将数据用设定好的大小传输
   3. AXI对应的 reader 和 writer 原生接口

