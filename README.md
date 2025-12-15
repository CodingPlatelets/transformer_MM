# Transformer-Based Large Language Model Accelerator (基于Transformer的大语言模型加速器)

[English Version](#english-version) | [中文版](#中文版)

For more detailed information about this project, visit: https://deepwiki.com/CodingPlatelets/transformer_MM

## English Version

### Overview
This is a comprehensive transformer-based large language model (LLM) accelerator developed using Chisel HDL. It implements specialized arithmetic logic units, memory controllers, and processing elements optimized for neural network computations commonly used in large language models. The design combines both floating-point and fixed-point arithmetic operations with high-efficiency systolic arrays.

### Architecture Components

#### 1. Processing Elements (PE)
Simple multiply-accumulate (MAC) units that form the foundation of systolic arrays, designed for efficient matrix operations. The PE components include:
- **PEFxp (Fixed-point PE)**: Supports fixed-point arithmetic with configurable bit widths
- **PEFp (Floating-point PE)**: IEEE-754 compliant floating-point MAC unit
- Input/output interfaces optimized for forming systolic array structures
- Pipelined operations for continuous data flow

#### 2. Arithmetic Logic Unit (ALU)

##### 2.1 Floating-Point Computation Unit
- IEEE-754 standard compliant single-precision floating-point operations
- Supported operations: Addition, subtraction, multiplication, division
- Cycle counts: 4 cycles for add/subtract, 2 cycles for multiply, 5 cycles for divide
- Pipeline stages: Uses fudian.FCMA (Fused Compare-Multiply-Add) modules with configurable exponent (8) and significand (24) widths for 32-bit floats
- Latency characteristics: Fully pipelined with 1 cycle throughput for multiply-add operations
- Note: Float-to-fixed conversion modules are under development

##### 2.2 Fixed-Point Computation Unit
- Configurable bit-width and decimal point positioning (WII: Integer bits, WIF: Fractional bits)
- Supported operations: Addition, subtraction, multiplication, division, square root, scaling/zoom
- **Addition/Subtraction**: 2 cycles total (combinational logic + 2-cycle FxpZoom scaling module)
- **Multiplication**: 2 cycles total (combinational logic + 2-cycle FxpZoom scaling module)
- **Division**: `WOI + WOF + 3` cycles total (based on iterative digit-recurrence algorithm)
- **Square root**: `(WII + 2 - 1) / 2 + WIF + 1` cycles (based on binary restoration algorithm)
- **Scaling/Zoom**: 2 cycles (for changing bit width or decimal point position)
- **SRT-16 Division Algorithm**: Approximately `(len + 3) / 2` cycles for integer division with remainder, where `len` is the operand length
- Rounding: Configurable rounding modes for precision control

##### 2.3 GEMM (General Matrix Multiply) Unit
- Systolic array algorithm implementation for `n × n` square matrix multiplication
- Total latency: Exactly 3n cycles for complete `n × n` matrix multiplication (as implemented in the counter logic)
- Operates on fixed-point UInt values, configurable data types (UInt, FixedPoint, FloatPoint32/64)
- **Processing Element (PE)**:
  - Fixed-point PEFxp: MAC unit with combinational multiply-add logic, accumulator register updated each cycle
  - Floating-point PEFp: Uses fudian.FCMA modules for IEEE-754 compliant operations
- **Systolic Array Control**:
  - Input staging: Initial matrix loading and staging in internal registers
  - Data propagation: 2n cycles for data injection into the systolic array
  - Computation phase: n cycles for complete data flow through the array
  - Total latency: Exactly 3n cycles (confirmed by `val cnt = Counter(3 * n)` in code)
  - Throughput: 1 result per cycle during computation phase after initial loading
- **Pipeline stages**: Each PE includes RegNext for horizontal and vertical data forwarding
- Optimized for neural network dense layer operations with configurable matrix sizes

##### 2.4 GEMV (General Matrix-Vector Multiply) Unit
- Reduction tree algorithm for vector-matrix multiplication using VecDotVec components
- **VecDotVec**:
  - Basic dot product of two size-n vectors
  - Pipelined tree reduction: Multiplies corresponding elements, then reduces using binary tree
  - Total latency: `log₂(n) + 1` cycles (for tree reduction with RegNext delay per level)
  - Each multiplication stage has 1 cycle delay through the reduction tree
- Supports both fixed-point UInt and FXP types
- Efficient for attention mechanism computations with variable vector sizes

##### 2.5 Softmax Computation Unit
- **Fixed-point exponential function**: Specialized implementation based on I-BERT algorithm (https://arxiv.org/abs/2101.01321)
  - Uses quadratic approximation: `f(x) = k₁(x + bias₁)² + bias₂` where `k₁=0.3585`, `bias₁=1.353`, `bias₂=0.344`
  - **Exponential latency**: Exactly 3 cycles (as defined by `expDelay = 3` in code)
- **Softmax pipeline**:
  - Stage 1: Find maximum value in input vector using parallel reduction tree (1 cycle)
  - Stage 2: Compute `exp(x - max)` for each element using the fixed-point exponential function (3 cycles + pipeline delay)
  - Stage 3: Sum all exponential results for normalization denominator (using reduceTree with RegNext per level)
  - Stage 4: Divide each exponential result by sum using FXP division module (`WOI + WOF + 3` cycles)
  - **Total latency**: Approximately 1 + 3 + log₂(arraySize) + (WOI + WOF + 3) cycles
- Implements numerically stable softmax with max-subtraction technique to prevent overflow
- Supports configurable array sizes (default 4 elements, configurable)

##### 2.6 Obsolete Components
- **SPMM (Sparse-Dense Matrix Multiplication)**: Performs sparse-dense matrix multiplication using MAC units with mask vectors to select needed elements
  - Uses mask vectors to select specific numbers from input matrices
  - Implements row-wise processing with configurable dimension parameters
  - Supports streaming data input with queue management
  - Core algorithm: A sparse L×L matrix with mask selection dot-products with specific rows of a V matrix selected by the same mask
  - Parameters: Configurable bit width, vector dimension (dimV), matrix size (L), number of masks (numOfMask)
  - Used in older attention mechanism implementations where sparse patterns needed efficient processing

- **SDDMM (Sampled Dense-Dense Matrix Multiplication)**: Computes sampled dense-dense matrix multiplication for attention computations
  - Calculates row-by-row dot products between two dense matrices with sampling via masks
  - Implements a tree reduction approach for efficient computation
  - Core algorithm: K-matrix rows dot product with input vectors, with results indexed by mask positions
  - Uses VecDotVecTree component for efficient tree-based reduction
  - Used in legacy attention computation flows where both matrices are dense but only selected elements needed

- **SDPMM (Sparse-Dense Product for Multi-head Attention)**: Combines SDDMM and SPMM for complete attention computation
  - Pipeline architecture: SDDMM (Score computation) → Softmax (planned) → SPMM (Value selection)
  - Processes attention mechanism in two stages: score computation followed by value aggregation
  - Supports multi-head attention with configurable mask counts
  - The 'SdpmmOrigin' variant includes forwarding delay memory for pipelined processing
  - Used in earlier transformer model implementations before more optimized approaches

These components are deprecated in favor of more efficient implementations but remain available for compatibility with existing projects and legacy codebases.

#### 3. Memory Components

##### 3.1 Custom SRAM Modules
- Configurable size and data type
- On-chip memory solution with low latency
- Requires sufficient available on-chip resources

##### 3.2 HBM/DRAM Streaming Interface
- High-bandwidth memory streaming functionality
- Automatic data slicing and transfer according to defined chunk sizes
- Bypasses burst length and AXI maximum bit width limitations

##### 3.3 AXI Interfaces
- Native AXI4 protocol interfaces
- Reader and writer modules for external memory access
- Compatible with FPGA memory systems

#### 4. Top-Level Integration
- **VitisRTLKernel**: Top-level module for integration with Xilinx platforms
- AXI-based control interface with start, done, idle, and ready signals
- State machine controlling initialization, execution, and completion phases

#### 5. Model Implementations
- **LLaMA 3 Support**: Includes attention layer and matrix controller implementations for LLaMA 3 architecture
- Transformer block designs optimized for inference

### Testing Framework

#### Running Tests
The project uses ChiselTest for verification. Run tests using the following commands:

**Specific Component Tests:**
```bash
# Using sbt directly - Specific Component Tests
sbt "testOnly kernel.alu.GEMMTest"
sbt "testOnly kernel.alu.SoftmaxTest"
sbt "testOnly kernel.alu.FXPArithmTest"
sbt "testOnly kernel.alu.IntDivTest"
sbt "testOnly kernel.alu.AverageModuleSpec"
```

**Run All Tests:**
```bash
# Using sbt for all tests
sbt test

# Or using Make command
make test
```

**Hardware Emulation Tests:**
```bash
# For Vitis kernel tests
sbt "testOnly vitisrtlkernel.VitisRTLKernelTest"
```

#### Test Coverage

**PE (Processing Element) Tests:**
- `PEFxpTest`: Fixed-point processing element validation
- `PEFpTest`: Floating-point processing element validation
- Tests MAC operations with various input sequences
- Validates accumulator functionality

**ALU Component Tests:**
- **GEMM Test**: Validates `n × n` matrix multiplication using reference CPU implementation
  - **Matrix Generation**: Uses `matInit(n)` function to generate random matrices with values in range [1, 6] using configurable seed
  - **Reference Implementation**: Implements `mmul(a, b)` software function that computes matrix multiplication using nested loops: `result[i][j] = sum(a[i][k] * b[k][j]) for k in 0 to n-1`
  - **Fixed-Point Test Process (`testGEMM` function)**:
    - Generates 10 pairs of random `n×n` matrices (configurable arraySize)
    - Converts floating-point values to fixed-point representation using `doubleToFixedPoint()` function
    - Provides matrices to hardware via `io.in_a` and `io.in_b` input interfaces when ready
    - Implements dual-threaded testing: one thread feeds input data, another checks output results
    - Compares hardware output with software reference with tolerance of 0.001
    - Verifies both fixed-point and floating-point data types (GEMMDataType.Fxp and GEMMDataType.Fp32)
  - **Example Test Case**: For a 4×4 matrix multiplication A×B=C:
    - Input: Two random 4×4 matrices (A and B) with values between 1.0-6.0
    - Processing: Hardware processes matrices through systolic array in 3n=12 cycles
    - Output: Hardware produces 16 result elements (4×4 result matrix C)
    - Validation: Each output element compared against software-calculated value C[i][j] = Σ A[i][k] * B[k][j]
  - **Test Configuration**: Supports different matrix sizes (n parameter), fixed-point configurations (I integer bits, F fractional bits), and data types
- **GEMV Test**: Vector-matrix multiplication validation
  - Tests reduction tree algorithm implementation
  - Verifies `log(n) + 2` cycle timing
- **Softmax Test**:
  - Fixed-point exponential function accuracy verification
  - Numerical stability testing with max-subtraction
  - Relative error maintained under 5% threshold
- **FXP Arithmetic Tests**: Fixed-point operation validation
  - Addition, subtraction, multiplication, division tests
  - Precision and overflow boundary checks
- **SRT16 Divider Test**: Integer division validation
  - Tests SRT-16 algorithm implementation
  - Verifies quotient and remainder calculations

**Memory Tests:**
- Custom SRAM module validation
- AXI interface read/write operations
- HBM/DRAM streaming functionality tests

**Complete System Tests:**
- End-to-end pipeline validation
- State machine operation verification
- AXI interface protocol compliance

#### Testing Methodology

**Input Generation:**
- Pseudo-random number generators with fixed seeds for reproducible results
- Matrix initialization with controlled value ranges
- Boundary condition testing with extreme values

**Validation Approach:**
- Hardware vs. software reference model comparison
- Cycle-accurate timing verification
- Bit-exact result validation for fixed-point operations
- Floating-point precision validation within tolerance bounds

**Metrics:**
- Numerical accuracy: Relative error percentages
- Performance: Cycles per operation verification
- Resource utilization: Estimated FPGA resource usage
- Throughput: Operations per second measurements

#### Test Configuration
Many tests allow parameterization of:
- Matrix sizes (for GEMM/GEMV operations)
- Fixed-point bit widths (integer and fractional portions)
- Test array sizes
- Precision thresholds

### Project Structure

#### Directory Layout
```
/Volumes/base/code/transformer_MM/
├── build.sbt                    # SBT build configuration
├── build.sc                     # Mill build configuration
├── Makefile                     # Make commands and flow definitions
├── LICENSE                      # Project licensing information
├── README.md                    # Original project documentation
├── readme-new.md                # This comprehensive documentation
├── mill                         # Mill build tool executable
├── dependencies/                # External library dependencies
├── xo_kernel/                   # Xilinx kernel object outputs
├── host/                        # Host application code for FPGA
├── lib/                         # Additional library files
├── RTLRef/                      # RTL reference implementations
├── src/
│   ├── main/
│   │   └── scala/              # Source code
│   │       ├── kernel/         # Core kernel implementations
│   │       │   ├── alu/        # Arithmetic Logic Units
│   │       │   ├── configs/    # Configuration files
│   │       │   ├── deprecated/ # Obsolete components
│   │       │   ├── utils/      # Utility modules
│   │       │   ├── LinearLayer.scala # Linear layer implementation
│   │       │   └── TOPSdpmm.scala    # Top-level SDPMM implementation
│   │       ├── models/         # Model-specific implementations
│   │       │   └── llama3/     # LLaMA 3 model support
│   │       └── vitiskernel/    # Vitis integration components
│   └── test/
│       ├── scala/              # Scala/Chisel tests
│       │   ├── kernel/
│       │   │   ├── alu/        # ALU component tests
│       │   │   ├── deprecated/ # Tests for deprecated components
│       │   │   └── utils/      # Utility module tests
│       │   ├── models/         # Model-specific tests
│       └── py/                 # Python-based tests (if any)
```

### Building and Generating RTL

#### Prerequisites
- Scala 2.13
- sbt 1.0+ or mill build tool
- Chisel 6.2.0
- Java Development Kit 8 or higher
- Xilinx Vitis/Vivado tools (for hardware deployment)
- GNU Make

#### Build Commands

**Compilation:**
```bash
# Using Make
make compile

# Or using sbt directly
sbt compile

# Using mill (if preferred)
mill __.compile
```

**Verilog Generation:**
```bash
# Generate Verilog output (using Make)
make verilog

# Or with sbt directly
sbt "runMain vitiskernel.VitisRTLKernelVerilog"

# Generated files location
./build/chisel/  # RTL output directory
```

**Testing:**
```bash
# Run all tests
make test

# Or run tests directly with sbt
sbt test

# Run specific test
sbt "testOnly kernel.alu.GEMMTest"
```

#### Build Flow

**Chisel Flow (Synthesizable RTL):**
- Source files: `src/main/scala/`
- Output directory: `./build/chisel/`
- Main generation target: `VitisRTLKernelVerilog`
- Uses CIRCT for SystemVerilog generation

**XCLBIN Flow (Hardware Binary):**
- Kernel object files: `./xo_kernel/`
- Build directory: `./build/xclbin/`
- Hardware emulation: `make hw_emu_xclbin`
- Hardware build: `make hw_xclbin`

**Host Application Flow:**
- Source: `./host/`
- Build directory: `./build/host/`
- Executable: `./build/host/host_executable`

#### Building for Hardware Deployment

**Hardware Emulation:**
```bash
# Prepare hardware emulation
make emconfig
make hw_emu_xclbin
make host
make run_emu
```

**Hardware Target:**
```bash
# Build for actual hardware
make hw_xclbin
make host
make run_hw
```

#### Development Commands
```bash
# Check code formatting
make checkformat

# Reformat code
make reformat

# Clean build artifacts
make clean

# Clean VPP build (hardware)
make clean_hw_vpp
make clean_emu_vpp
```

### 硬件目标集成
- 针对Xilinx FPGA平台设计（特别测试了xilinx_u280_gen3x16_xdma_1_202211_1）
- AXI4兼容的内存和控制接口
- 与Vitis开发环境兼容
- 支持硬件仿真和综合
- 支持硬件仿真（hw_emu）和硬件（hw）目标

---

## 中文版

欲了解更多关于此项目的详细信息，请访问：https://deepwiki.com/CodingPlatelets/transformer_MM

### 概述

### 测试框架

#### 运行测试
项目使用ChiselTest进行验证。使用以下方式运行测试：

**特定组件测试：**
```bash
# 使用sbt直接运行 - 特定组件测试
sbt "testOnly kernel.alu.GEMMTest"
sbt "testOnly kernel.alu.SoftmaxTest"
sbt "testOnly kernel.alu.FXPArithmTest"
sbt "testOnly kernel.alu.IntDivTest"
sbt "testOnly kernel.alu.AverageModuleSpec"
```

**运行所有测试：**
```bash
# 使用sbt进行所有测试
sbt test

# 或使用Make命令
make test
```

**硬件仿真测试：**
```bash
# 运行Vitis内核实例测试
sbt "testOnly vitisrtlkernel.VitisRTLKernelTest"
```

#### 测试覆盖

**PE (处理元素) 测试：**
- `PEFxpTest`: 定点处理元素验证
- `PEFpTest`: 浮点处理元素验证
- 使用各种输入序列测试MAC操作
- 验证累加器功能

**ALU组件测试：**
- **GEMM测试**: 使用参考CPU实现验证 `n × n` 矩阵乘法
  - **矩阵生成**: 使用 `matInit(n)` 函数生成值范围在 [1, 6] 内的随机矩阵，使用可配置种子
  - **参考实现**: 实现 `mmul(a, b)` 软件函数，使用嵌套循环计算矩阵乘法：`result[i][j] = sum(a[i][k] * b[k][j]) for k in 0 to n-1`
  - **定点测试过程 (`testGEMM` 函数)**:
    - 生成10对随机 `n×n` 矩阵（可配置arraySize）
    - 使用 `doubleToFixedPoint()` 函数将浮点值转换为定点表示
    - 在就绪时通过 `io.in_a` 和 `io.in_b` 输入接口向硬件提供矩阵
    - 实现双线程测试：一个线程提供输入数据，另一个线程检查输出结果
    - 使用0.001容差将硬件输出与软件参考进行比较
    - 验证定点和浮点数据类型（GEMMDataType.Fxp和GEMMDataType.Fp32）
  - **示例测试案例**: 对于 4×4 矩阵乘法 A×B=C:
    - 输入: 两个随机4×4矩阵（A和B），值介于1.0-6.0之间
    - 处理: 硬件通过脉动阵列在3n=12周期内处理矩阵
    - 输出: 硬件产生16个结果元素（4×4结果矩阵C）
    - 验证: 每个输出元素与软件计算值 C[i][j] = Σ A[i][k] * B[k][j] 进行比较
  - **测试配置**: 支持不同矩阵大小（n参数）、定点配置（I整数位，F小数位）和数据类型
- **GEMV测试**: 向量-矩阵乘法验证
  - 测试归约树算法实现
  - 验证 `log(n) + 2` 周期时序
- **Softmax测试**:
  - 定点指数函数精度验证
  - 使用最大值减法的数值稳定性测试
  - 相对误差保持在5%阈值以下
- **FXP算术测试**: 定点运算验证
  - 加法、减法、乘法、除法测试
  - 精度和溢出边界检查
- **SRT16除法测试**: 整数除法验证
  - 测试SRT-16算法实现
  - 验证商和余数计算

**内存测试：**
- 自定义SRAM模块验证
- AXI接口读写操作
- HBM/DRAM流式功能测试

**完整系统测试：**
- 端到端流水线验证
- 状态机操作验证
- AXI接口协议兼容性

#### 测试方法

**输入生成：**
- 使用固定种子的伪随机数生成器以获得可重现的结果
- 具有受控值范围的矩阵初始化
- 使用极值的边界条件测试

**验证方法：**
- 硬件与软件参考模型比较
- 周期精确时序验证
- 定点运算的位精确结果验证
- 浮点精度在容差范围内的验证

**指标：**
- 数值精度：相对误差百分比
- 性能：每操作周期数验证
- 资源利用率：估计的FPGA资源使用量
- 吞吐量：每秒操作数测量

#### 测试配置
许多测试允许参数化：
- 矩阵大小（用于GEMM/GEMV操作）
- 定点位宽（整数和小数部分）
- 测试数组大小
- 精度阈值

### 项目结构

#### 目录布局
```
/Volumes/base/code/transformer_MM/
├── build.sbt                    # SBT构建配置
├── build.sc                     # Mill构建配置
├── Makefile                     # Make命令和流程定义
├── LICENSE                      # 项目许可信息
├── README.md                    # 原始项目文档
├── readme-new.md                # 本综合文档
├── mill                         # Mill构建工具执行文件
├── dependencies/                # 外部库依赖
├── xo_kernel/                   # Xilinx内核实例输出
├── host/                        # FPGA的主机应用程序代码
├── lib/                         # 额外库文件
├── RTLRef/                      # RTL参考实现
├── src/
│   ├── main/
│   │   └── scala/              # 源代码
│   │       ├── kernel/         # 核心内核实例
│   │       │   ├── alu/        # 算术逻辑单元
│   │       │   ├── configs/    # 配置文件
│   │       │   ├── deprecated/ # 已废弃的组件
│   │       │   ├── utils/      # 实用模块
│   │       │   ├── LinearLayer.scala # 线性层实现
│   │       │   └── TOPSdpmm.scala    # 顶层SDPMM实现
│   │       ├── models/         # 模型特定的实现
│   │       │   └── llama3/     # LLaMA 3模型支持
│   │       └── vitiskernel/    # Vitis集成组件
│   └── test/
│       ├── scala/              # Scala/Chisel测试
│       │   ├── kernel/
│       │   │   ├── alu/        # ALU组件测试
│       │   │   ├── deprecated/ # 已废弃组件的测试
│       │   │   └── utils/      # 实用模块测试
│       │   ├── models/         # 模型特定的测试
│       └── py/                 # Python测试（如果有）
```

### 构建和生成RTL

#### 先决条件
- Scala 2.13
- sbt 1.0+ 或 mill构建工具
- Chisel 6.2.0
- Java开发工具包8或更高版本
- Xilinx Vitis/Vivado工具（用于硬件部署）
- GNU Make

#### 构建命令

**编译：**
```bash
# 使用Make
make compile

# 或直接使用sbt
sbt compile

# 使用mill（如果首选）
mill __.compile
```

**Verilog生成：**
```bash
# 生成Verilog输出（使用Make）
make verilog

# 或直接使用sbt
sbt "runMain vitiskernel.VitisRTLKernelVerilog"

# 生成文件位置
./build/chisel/  # RTL输出目录
```

**测试：**
```bash
# 运行所有测试
make test

# 或直接使用sbt运行测试
sbt test

# 运行特定测试
sbt "testOnly kernel.alu.GEMMTest"
```

#### 构建流程

**Chisel流程（可综合RTL）：**
- 源文件：`src/main/scala/`
- 输出目录：`./build/chisel/`
- 主要生成目标：`VitisRTLKernelVerilog`
- 使用CIRCT生成SystemVerilog

**XCLBIN流程（硬件二进制）：**
- 内核实例文件：`./xo_kernel/`
- 构建目录：`./build/xclbin/`
- 硬件仿真：`make hw_emu_xclbin`
- 硬件构建：`make hw_xclbin`

**主机应用程序流程：**
- 源代码：`./host/`
- 构建目录：`./build/host/`
- 可执行文件：`./build/host/host_executable`

#### 硬件部署构建

**硬件仿真：**
```bash
# 准备硬件仿真
make emconfig
make hw_emu_xclbin
make host
make run_emu
```

**硬件目标：**
```bash
# 为实际硬件构建
make hw_xclbin
make host
make run_hw
```

#### 开发命令
```bash
# 检查代码格式
make checkformat

# 重新格式化代码
make reformat

# 清理构建产物
make clean

# 清理VPP构建（硬件）
make clean_hw_vpp
make clean_emu_vpp
```

### 概述
这是一个基于Chisel HDL开发的全面的Transformer大语言模型加速器。该设计实现了专门针对大语言模型常用神经网络计算优化的算术逻辑单元、存储控制器和处理元素。设计结合了浮点和定点运算，并采用高效脉动阵列。

### 架构组件

#### 1. 处理元素 (PE)
构成脉动阵列基础的简单乘累加(MAC)单元，专为高效的矩阵运算设计。PE组件包括：
- **PEFxp (定点PE)**: 支持可配置位宽的定点运算
- **PEFp (浮点PE)**: 符合IEEE-754标准的浮点MAC单元
- 针对构建脉动阵列结构优化的输入/输出接口
- 用于连续数据流的流水线操作

#### 2. 算术逻辑单元 (ALU)

##### 2.1 浮点运算单元
- 符合IEEE-754标准的单精度浮点运算
- 支持的操作：加法、减法、乘法、除法
- 周期数：加/减法4周期，乘法2周期，除法5周期
- 流水线阶段：使用fudian.FCMA（融合比较-乘法-加法）模块，为32位浮点数配置指数（8位）和尾数（24位）宽度
- 延迟特性：完全流水线化，乘-加操作具有1周期吞吐量
- 注意：浮点到定点转换模块正在开发中

##### 2.2 定点运算单元
- 可配置的位宽和小数点定位（WII：整数位，WIF：小数位）
- 支持的操作：加法、减法、乘法、除法、平方根、缩放/转换
- **加法/减法**：2周期总时长（组合逻辑 + 2周期FxpZoom缩放模块）
- **乘法**：2周期总时长（组合逻辑 + 2周期FxpZoom缩放模块）
- **除法**：`WOI + WOF + 3` 总周期数（基于迭代数字递归算法）
- **平方根**：`(WII + 2 - 1) / 2 + WIF + 1` 周期（基于二进制恢复算法）
- **缩放/转换**：2周期（用于更改位宽或小数点位置）
- **SRT-16除法算法**：约 `(len + 3) / 2` 周期的带余数整数除法，其中 `len` 是操作数长度
- 舍入：可配置的舍入模式用于精度控制

##### 2.3 GEMM (通用矩阵乘法) 单元
- 脉动阵列算法实现 `n × n` 方矩阵乘法
- 总延迟：确切3n周期完成 `n × n` 矩阵乘法（如代码计数器逻辑实现）
- 操作于定点UInt值，可配置数据类型（UInt、FixedPoint、FloatPoint32/64）
- **处理元素 (PE)**：
  - 定点PEFxp：MAC单元，组合乘-加逻辑，累加器寄存器每周期更新
  - 浮点PEFp：使用fudian.FCMA模块实现IEEE-754兼容操作
- **脉动阵列控制**：
  - 输入暂存：初始矩阵加载和暂存在内部寄存器
  - 数据传播：2n周期用于数据注入脉动阵列
  - 计算阶段：n周期用于数据完整流经阵列
  - 总延迟：确切3n周期（由代码中`val cnt = Counter(3 * n)`确认）
  - 吞吐量：计算阶段每周期1个结果（初始加载后）
- **流水线阶段**：每个PE包含RegNext用于水平和垂直数据转发
- 针对神经网络密集层操作优化，具有可配置矩阵大小

##### 2.4 GEMV (通用矩阵-向量乘法) 单元
- 使用VecDotVec组件的归约树算法进行向量-矩阵乘法
- **VecDotVec**：
  - 两个n维向量的基本点积
  - 流水线树归约：乘以对应元素，然后使用二叉树归约
  - 总延迟：`log₂(n) + 1` 周期（用于树归约，每级有RegNext延迟）
  - 每个乘法阶段在归约树中都有1周期延迟
- 支持定点UInt和FXP类型
- 适用于可变向量大小的注意力机制计算

##### 2.5 Softmax运算单元
- **定点指数函数**：基于I-BERT算法的专用实现 (https://arxiv.org/abs/2101.01321)
  - 使用二次逼近：`f(x) = k₁(x + bias₁)² + bias₂` 其中 `k₁=0.3585`，`bias₁=1.353`，`bias₂=0.344`
  - **指数延迟**：确切3周期（如代码中`expDelay = 3`定义）
- **Softmax流水线**：
  - 第1阶段：使用并行归约树在输入向量中查找最大值（1周期）
  - 第2阶段：使用定点指数函数计算每个元素的 `exp(x - max)`（3周期+流水线延迟）
  - 第3阶段：将所有指数结果求和作为归一化分母（使用reduceTree，每级有RegNext延迟）
  - 第4阶段：使用FXP除法模块将每个指数结果除以总和（`WOI + WOF + 3` 周期）
  - **总延迟**：约 1 + 3 + log₂(数组大小) + (WOI + WOF + 3) 周期
- 实现数值稳定的softmax，采用最大值减法技巧防止溢出
- 支持可配置数组大小（默认4个元素，可配置）

##### 2.6 已废弃组件
- **SPMM (稀疏-稠密矩阵乘法)**：使用MAC单元和掩码向量执行稀疏-稠密矩阵乘法来选择所需元素
  - 使用掩码向量从输入矩阵中选择特定数字
  - 实现行式处理，具有可配置的维度参数
  - 支持带队列管理的流数据输入
  - 核心算法：稀疏L×L矩阵与掩码选择的V矩阵特定行进行点积运算
  - 参数：可配置位宽、向量维度(dimV)、矩阵大小(L)、掩码数量(numOfMask)
  - 用于较早的注意力机制实现，其中稀疏模式需要高效处理

- **SDDMM (采样稠密-稠密矩阵乘法)**：为注意力计算执行采样稠密-稠密矩阵乘法
  - 使用掩码采样计算两个稠密矩阵的逐行点积
  - 实现树归约方法以提高计算效率
  - 核心算法：K-矩阵行与输入向量的点积，结果按掩码位置索引
  - 使用VecDotVecTree组件实现高效的基于树的归约
  - 用于传统注意力计算流程，其中两个矩阵都是稠密的但只需要选择部分元素

- **SDPMM (多头注意力的稀疏-稠密乘积)**：结合SDDMM和SPMM进行完整的注意力计算
  - 流水线架构：SDDMM（分数计算）→ Softmax（计划中）→ SPMM（值选择）
  - 分两个阶段处理注意力机制：分数计算后跟值聚合
  - 支持带可配置掩码计数的多头注意力
  - 'SdpmmOrigin'变体包含前馈延迟存储器用于流水线处理
  - 用于更优化方法之前的早期Transformer模型实现

这些组件因更高效的实现而弃用，但为与现有项目和传统代码库的兼容性而保留。

#### 3. 存储组件

##### 3.1 自定义SRAM模块
- 可配置大小和数据类型
- 低延迟片上内存解决方案
- 需要足够的可用片上资源

##### 3.2 HBM/DRAM流式接口
- 高带宽内存流式功能
- 根据定义的块大小自动数据切片和传输
- 绕过突发长度和AXI最大位宽限制

##### 3.3 AXI接口
- 原生AXI4协议接口
- 外部内存访问的读写模块
- 与FPGA内存系统兼容

#### 4. 顶层集成
- **VitisRTLKernel**: 与Xilinx平台集成的顶层模块
- 基于AXI的控制接口，带有开始、完成、空闲和就绪信号
- 控制初始化、执行和完成阶段的状态机

#### 5. 模型实现
- **LLaMA 3支持**: 包含LLaMA 3架构的注意力层和矩阵控制器实现
- 针对推理优化的Transformer块设计

### 测试框架

#### 运行测试
项目使用ChiselTest进行验证。使用以下方式运行测试：
```bash
# 使用sbt
sbt "testOnly kernel.alu.GEMMTest"
sbt "testOnly kernel.alu.SoftmaxTest"
sbt "testOnly kernel.alu.FXPArithmTest"

# 或使用Make
make test
```

#### 测试覆盖
- **GEMM测试**: 使用参考实现进行矩阵乘法验证
- **Softmax测试**: 相对误差检查的数值精度验证（<5%阈值）
- **FXP算术测试**: 定点运算验证
- **整数除法测试**: SRT-16除法算法验证
- **PE测试**: 单个处理元素正确性检查

#### 测试方法
- 随机和确定性输入生成
- 参考模型比较（基于CPU的计算）
- 精度评估指标
- 时序验证

### 构建和生成RTL

#### 先决条件
- Scala 2.13
- sbt或mill（Mill构建工具）
- Chisel 6.2.0
- Java开发工具包

#### Verilog生成
```bash
# 生成Verilog输出
make verilog
# 或直接使用sbt:
sbt "runMain vitiskernel.VitisRTLKernelVerilog"
```

#### 编译
```bash
# 编译项目
make compile
# 或使用sbt:
sbt compile
```

### 硬件目标集成
- 针对Xilinx FPGA平台设计
- AXI4兼容接口
- 与Vitis开发环境兼容
- 支持硬件仿真和综合