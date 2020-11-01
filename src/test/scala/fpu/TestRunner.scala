package fpu

import scala.sys.process._
import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import chisel3._
import fpu.RoudingMode.{RMM, RNE}
import fpu.TestGen.{FpuTest, rmAll, rmMap}
import fpu.util.FPUDebug


object TestRunner extends App {

  override def main(args: Array[String]): Unit = {
    val add64 = Seq(FpuTest("f64_add", rmAll, backend = "verilator", pipeline = false))
    val sub64 = Seq(FpuTest("f64_sub", rmAll, backend = "verilator", pipeline = false))
    val mul64 = Seq(FpuTest("f64_mul", rmAll, backend = "verilator", pipeline = false))
    val fma64 = Seq(FpuTest("f64_mulAdd", Seq(RNE), pipeline = false))

    val tests = add64 ++ sub64 ++ mul64

    //  val tests = fadd_tests ++ fmul_tests ++
    //    sqrt_tests ++ div_tests ++
    //      fcmp_tests ++ i2f_tests ++
    //      f2i_tests ++ f2f_tests
    def testFilter(x: (File, FpuTest)) : Boolean = {
      true
//      x._1.getName == "f64_add-rnear_even-1-00"
    }

//    var filesAndConfigs = Seq[(File, FpuTest)]()
//
//    for(t <- tests){
//      for(rm <- t.roundingModes){
//        val files = TestGen.generate(test = t.name, rm = rmMap(rm), seed = 1)
//        val configs = files.indices.map(_ => t.copy(roundingModes = Seq(rm)))
//        filesAndConfigs ++= files.zip(configs)
//      }
//    }
//    filesAndConfigs = filesAndConfigs.filter(testFilter)

    val filesAndConfigs = tests.flatMap(t => {
      t.roundingModes.flatMap(rm => {
        val files = TestGen.generate(test = t.name, rm = rmMap(rm))
        val configs = files.map(_ => t.copy(roundingModes = Seq(rm)))
        files.zip(configs)
      })
    })

    println(filesAndConfigs.size)

    val threads = if(args.isEmpty) 1 else args.head.toInt
    val pool = java.util.concurrent.Executors.newFixedThreadPool(threads)

//    FPUDebug.flag = true

    val finished = new AtomicInteger(0)

    val startTime = System.nanoTime()

    for (((f, t), i) <- filesAndConfigs.zipWithIndex) {
      pool.execute(new Runnable {
        override def run(): Unit = {
          val tester = new FPUSubModuleTester(f, t)
          org.scalatest.run(tester)
          finished.getAndIncrement()
        }
      })
    }
    pool.shutdown()

    val progressLog = new File("./debug/progress.log")
    while (!pool.isTerminated){
      Thread.sleep(1)
      val elapsedTime = System.nanoTime() - startTime
      val progress = s" finished: ${finished.get()} / ${filesAndConfigs.size}" +
        s" elapsed: ${elapsedTime / 1e9} s"
      (s"echo $progress" #> progressLog).!
    }
  }
}

