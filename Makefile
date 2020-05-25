ARGS ?= -X verilog --target-dir build

verilog:
	sbt 'run $(ARGS)'

clean:
	rm -rf ./build/*
