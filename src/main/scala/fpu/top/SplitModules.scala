package fpu.top

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import fpu.divsqrt.DivSqrt
import fpu.{F32toF64, F64toF32, FCMP, FMV, FloatToInt, IntToFloat}
import fpu.fma.{FMA, FMANew, FMAv2}

object SplitModules extends App {

  val modules = Seq(
    () => new FMA,
    () => new FCMP,
    () => new FMV,
    () => new FloatToInt,
    () => new IntToFloat,
    () => new F32toF64,
    () => new F64toF32,
    () => new DivSqrt
  )

  val fma = Seq(() => new FMANew(enableDebug = false))

  val stage = new ChiselStage

  for(m <- fma){
    stage.execute(args, Seq(new ChiselGeneratorAnnotation(m)))
  }

}
