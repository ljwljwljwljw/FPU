package fpu

import java.io.File

import scala.sys.process._
import chisel3._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation}
import fpu.RoudingMode.{RDN, RMM, RNE, RTZ, RUP}
import fpu.divsqrt.DivSqrt
import fpu.fma.{FMA, FMANew, FMAPipeline}
import fpu.FPUOpType._

object TestGen
{
  val rmAll = Seq(RNE, RTZ, RUP, RDN, RMM)

  case class FpuTest
  (
    name: String,
    roundingModes: Seq[UInt],
    backend: String = "verilator",
    writeVcd: Boolean = false,
    pipeline: Boolean = true
  )

  val sqrt_tests = Seq(
    FpuTest("f64_sqrt", rmAll, pipeline = false),
    FpuTest("f32_sqrt", rmAll, pipeline = false)
  )
  val div_tests = Seq(
    FpuTest("f64_div", rmAll, pipeline = false),
    FpuTest("f32_div", rmAll, pipeline = false)
  )

  val i2f_tests = Seq(
    FpuTest("i32_to_f64", rmAll),
    FpuTest("ui32_to_f64", rmAll),
    FpuTest("i64_to_f64", rmAll),
    FpuTest("ui64_to_f64", rmAll),
    FpuTest("i32_to_f32", rmAll),
    FpuTest("ui32_to_f32", rmAll),
    FpuTest("i64_to_f32", rmAll),
    FpuTest("ui64_to_f32", rmAll)
  )

  val f2i_tests = Seq(
    FpuTest("f64_to_i64", rmAll),
    FpuTest("f64_to_ui64", rmAll),
    FpuTest("f32_to_i64", rmAll),
    FpuTest("f32_to_ui64", rmAll),
    FpuTest("f64_to_i32", rmAll),
    FpuTest("f64_to_ui32", rmAll),
    FpuTest("f32_to_i32", rmAll),
    FpuTest("f32_to_ui32", rmAll)
  )

  val f2f_tests = Seq(
    FpuTest("f64_to_f32", rmAll),
    FpuTest("f32_to_f64", rmAll)
  )

  val fcmp_tests = Seq(
    FpuTest("f64_le", rmAll),
    FpuTest("f64_lt", rmAll),
    FpuTest("f64_eq", rmAll),
    FpuTest("f32_le", rmAll),
    FpuTest("f32_lt", rmAll),
    FpuTest("f32_eq", rmAll)

  )

  val fma_tests = Seq(
    FpuTest("f64_mulAdd", rmAll),
    FpuTest("f32_mulAdd", rmAll)
  )

  val fadd_tests = Seq(
    FpuTest("f64_add", rmAll),
    FpuTest("f64_sub", rmAll),
    FpuTest("f32_add", rmAll),
    FpuTest("f32_sub", rmAll)
  )

  val fmul_tests = Seq(
    FpuTest("f64_mul", rmAll),
    FpuTest("f32_mul", rmAll)
  )
  val backendMap = Map(
    "treadle" -> TreadleBackendAnnotation,
    "verilator" -> VerilatorBackendAnnotation
  )
  val rmMap = Map(
    RoudingMode.RNE -> "rnear_even",
    RoudingMode.RTZ -> "rminMag",
    RoudingMode.RUP -> "rmax",
    RoudingMode.RDN -> "rmin",
    RoudingMode.RMM -> "rnear_maxMag"
  )

  def DivSqrt = new DivSqrt
  def IntToFloat = new IntToFloat
  def FloatToInt = new FloatToInt
  def F32ToF64 = new F32toF64
  def F64ToF32 = new F64toF32
  def FCMP = new FCMP
  def FMA = new FMAPipeline

  def D = true
  def S = false

  val testMap = Map[String, (() => FPUSubModule, Boolean, UInt)](elems =
    "f64_sqrt" -> (DivSqrt _, D, fsqrt),
    "f32_sqrt" -> (DivSqrt _, S, fsqrt),
    "f64_div" -> (DivSqrt _, D, fdiv),
    "f32_div" -> (DivSqrt _, S, fdiv),

    "i32_to_f64" -> (IntToFloat _, D, w2f),
    "ui32_to_f64" -> (IntToFloat _, D, wu2f),
    "i64_to_f64" -> (IntToFloat _, D, l2f),
    "ui64_to_f64" -> (IntToFloat _, D, lu2f),
    "i32_to_f32" -> (IntToFloat _, S, w2f),
    "ui32_to_f32" -> (IntToFloat _, S, wu2f),
    "i64_to_f32" -> (IntToFloat _, S, l2f),
    "ui64_to_f32" -> (IntToFloat _, S, lu2f),

    "f64_to_i32" -> (FloatToInt _, D, f2w),
    "f32_to_i32" -> (FloatToInt _, S, f2w),
    "f64_to_ui32" -> (FloatToInt _, D, f2wu),
    "f32_to_ui32" -> (FloatToInt _, S, f2wu),
    "f64_to_i64" -> (FloatToInt _, D, f2l),
    "f32_to_i64" -> (FloatToInt _, S, f2l),
    "f64_to_ui64" -> (FloatToInt _, D, f2lu),
    "f32_to_ui64" -> (FloatToInt _, S, f2lu),

    // 'isDouble' was not used in FloatToFloat
    "f32_to_f64" -> (F32ToF64 _, D, s2d),
    "f64_to_f32" -> (F64ToF32 _, D, d2s),

    "f64_le" -> (FCMP _, D, fle),
    "f64_lt" -> (FCMP _, D, flt),
    "f64_eq" -> (FCMP _, D, feq),
    "f32_le" -> (FCMP _, S, fle),
    "f32_lt" -> (FCMP _, S, flt),
    "f32_eq" -> (FCMP _, S, feq),

    "f64_add" -> (FMA _, D, fadd),
    "f32_add" -> (FMA _, S, fadd),
    "f64_sub" -> (FMA _, D, fsub),
    "f32_sub" -> (FMA _, S, fsub),
    "f64_mul" -> (FMA _, D, fmul),
    "f32_mul" -> (FMA _, S, fmul),

    "f64_mulAdd" -> (FMA _, D, fmadd),
    "f32_mulAdd" -> (FMA _, S, fmadd)
  )



  val dir = "./debug/tests"

  def generate
  (
    test: String = "f64_mulAdd",
    rm: String = "-rnear_even",
    seed: Int = 1
  ): Array[File] = {

    val cmd_mkdir = s"mkdir -p $dir"
    var ret = cmd_mkdir.!
    assert(ret == 0)

    val file_dir = new File(dir)
    assert(file_dir.exists() && file_dir.isDirectory)

    val testfloat_gen = "./debug/testfloat_gen "
    val flags = s"-$rm -seed $seed -exact -tininessafter"
    val cmd_genTest = s"$testfloat_gen $test $flags"
    val cmd_split = s"split -l 10000 -d - $dir/$test-$rm-$seed-"

    println(cmd_genTest + " | " + cmd_split)
    ret = (cmd_genTest #| cmd_split).!
    assert(ret == 0)

    val files = file_dir
      .listFiles()
      .filter(_.isFile)
      .filter(x => x.getName.startsWith(s"$test-$rm-$seed-"))
    files
  }


}
