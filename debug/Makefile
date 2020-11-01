TEST_TOP = fpu.TestRunner

ARGS ?= 

.DEFAULT_GOAL = unit-test

berkeley-softfloat-3/extract.stamp:
	rm -rf berkeley-softfloat-3
	git clone git://github.com/ucb-bar/berkeley-softfloat-3.git
	touch berkeley-softfloat-3/extract.stamp

berkeley-testfloat-3/extract.stamp:
	rm -rf berkeley-testfloat-3
	git clone git://github.com/ucb-bar/berkeley-testfloat-3.git
	touch berkeley-testfloat-3/extract.stamp

berkeley-softfloat-3/build/Linux-x86_64-GCC/softfloat.a: berkeley-softfloat-3/extract.stamp
	$(MAKE) -C berkeley-softfloat-3/build/Linux-x86_64-GCC SPECIALIZE_TYPE=RISCV

berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen: berkeley-testfloat-3/extract.stamp \
                                                           berkeley-softfloat-3/build/Linux-x86_64-GCC/softfloat.a
	$(MAKE) -C berkeley-testfloat-3/build/Linux-x86_64-GCC SPECIALIZE_TYPE=RISCV

./testfloat_gen: berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen
	cp berkeley-testfloat-3/build/Linux-x86_64-GCC/testfloat_gen .

unit-test: ./testfloat_gen
	@#cd ../ && mill FPU.test.testOnly -o -s $(TEST_TOP) $(ARGS)
	cd ../ && mill FPU.test.runMain $(TEST_TOP) $(ARGS)

clean:
	rm -rf ./tests
	rm ./*.log
	rm -rf ../test_run_dir
