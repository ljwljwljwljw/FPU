ARGS ?= -X verilog --target-dir build

TOP = fpu.top.DummyTop

BUILD_DIR = ./build

help:
	mill FPU.runMain $(TOP) --help

verilog:
	mill FPU.runMain $(TOP) -X verilog  -td $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)
