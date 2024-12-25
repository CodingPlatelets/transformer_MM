CHISEL_BUILD_DIR = ./build/chisel

############################## Chisel Flow #############################
test:
	#mill -i __.test.testOnly vitisrtlkernel.VitisRTLKernelTest
	sbt "testOnly vitisrtlkernel.VitisRTLKernelTest"

verilog:
	mkdir -p $(CHISEL_BUILD_DIR)
	#mill -i chiselVitisTemplate.runMain --mainClass vitisrtlkernel.VitisRTLKernelVerilog -td $(CHISEL_BUILD_DIR)
	# sbt run
	sbt "runMain vitisrtlkernel.VitisRTLKernelVerilog"

feature:
	sbt -J-Xmx50G "runMain kernel.NewFeatureTest"

help:
	mill -i __.runMain --mainClass vitisrtlkernel.VitisRTLKernelVerilog --help

compile:
	#mill -i __.compile
	sbt compile

bsp:
	mill -i mill.bsp.BSP/install

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(CHISEL_BUILD_DIR)

.PHONY: test verilog help compile bsp reformat checkformat clean

############################## XCLBIN Flow #############################

XCLBIN_BUILD_DIR = ./build/xclbin

EMU_DIR = hw_emu
HW_DIR = hw

XBUTIL = xbutil

HW_XCLBIN_TEMP_DIR = $(XCLBIN_BUILD_DIR)/$(HW_DIR)/tmp
HW_XCLBIN_LOG_DIR = $(XCLBIN_BUILD_DIR)/$(HW_DIR)/log
HW_XCLBIN_REPORT_DIR = $(XCLBIN_BUILD_DIR)/$(HW_DIR)/report

EMU_XCLBIN_TEMP_DIR = $(XCLBIN_BUILD_DIR)/$(EMU_DIR)/tmp
EMU_XCLBIN_LOG_DIR = $(XCLBIN_BUILD_DIR)/$(EMU_DIR)/log
EMU_XCLBIN_REPORT_DIR = $(XCLBIN_BUILD_DIR)/$(EMU_DIR)/report

VPP = v++
KERNEL_XO = ./xo_kernel/$(XO).xo
LINK_CFG = ./xo_kernel/$(XO).cfg

HW_XCLBIN = $(XCLBIN_BUILD_DIR)/hw/$(XO).xclbin

EMU_XCLBIN = $(XCLBIN_BUILD_DIR)/hw_emu/$(XO).xclbin

reset_device:
	$(XBUTIL) reset --device 0000:3d:00.1

hw_emu_xclbin: $(KERNEL_XO) $(LINK_CFG)
	mkdir -p $(EMU_XCLBIN_TEMP_DIR)
	mkdir -p $(EMU_XCLBIN_LOG_DIR)
	mkdir -p $(EMU_XCLBIN_REPORT_DIR)
	mkdir -p ./xo_kernel/$(EMU_DIR)
	$(VPP) -t hw_emu \
	-g \
	--temp_dir $(EMU_XCLBIN_TEMP_DIR) --save-temps --log_dir $(EMU_XCLBIN_LOG_DIR) --report_dir $(EMU_XCLBIN_REPORT_DIR) \
	--link $(KERNEL_XO) \
	--config $(LINK_CFG) -o ./xo_kernel/hw_emu/$(XO).xclbin

hw_xclbin: $(KERNEL_XO) $(LINK_CFG)
	mkdir -p $(HW_XCLBIN_TEMP_DIR)
	mkdir -p $(HW_XCLBIN_LOG_DIR)
	mkdir -p $(HW_XCLBIN_REPORT_DIR)
	mkdir -p ./xo_kernel/$(HW_DIR)
	$(VPP) -t hw \
	-R2 \
	--temp_dir $(HW_XCLBIN_TEMP_DIR) --save-temps --log_dir $(HW_XCLBIN_LOG_DIR) --report_dir $(HW_XCLBIN_REPORT_DIR) \
	--link $(KERNEL_XO) \
	--config $(LINK_CFG) -o ./xo_kernel/hw/$(XO).xclbin


clean_hw_vpp :
	-rm -rf $(HW_XCLBIN_TEMP_DIR)
	-rm -rf $(HW_XCLBIN_LOG_DIR)
	-rm -rf $(HW_XCLBIN_REPORT_DIR)
	-rm -rf ./.ipcaches

clean_emu_vpp :
	-rm -rf $(EMU_XCLBIN_TEMP_DIR)
	-rm -rf $(EMU_XCLBIN_LOG_DIR)
	-rm -rf $(EMU_XCLBIN_REPORT_DIR)
	-rm -rf ./.ipcaches

.PHONY: xclbin clean_vpp 

############################## Host Flow #############################

HOST_BUILD_DIR = ./build/host

HOST_SRC = ./host/*.cpp
HOST_INCLUDE = ./host/includes
Emconfigutil := emconfigutil

HOST_EXECUTABLE = $(HOST_BUILD_DIR)/host_executable

CXX := g++-10
CXXFLAGS += -g -std=c++17 -Wall
LDFLAGS += -I$(HOST_INCLUDE) -I$(XILINX_XRT)/include -L$(XILINX_XRT)/lib -lxrt_coreutil -lOpenCL -lpthread -lrt -lstdc++

emconfig:
	$(Emconfigutil) --platform xilinx_u280_gen3x16_xdma_1_202211_1 --od $(HOST_BUILD_DIR)

host: $(HOST_SRC)
	mkdir -p $(HOST_BUILD_DIR)
	$(CXX) $(HOST_SRC)  -o $(HOST_EXECUTABLE) $(CXXFLAGS) $(LDFLAGS)

run_emu: host $(HOST_EXECUTABLE)
	export XCL_EMULATION_MODE=hw_emu
	$(HOST_EXECUTABLE) ./xo_kernel/hw_emu/$(XCLBIN).xclbin

run_hw: host $(HOST_EXECUTABLE)
	$(HOST_EXECUTABLE) ./xo_kernel/hw/$(XCLBIN).xclbin

kill_xsim:
	ps aux | grep xsim | awk '{print $$2}' | xargs kill -9

DEV_XVC_PUB := /dev/xvc_pub.u0
hw_debug:
	xvc_pcie -d $(DEV_XVC_PUB)

.PHONY: host run hw_debug