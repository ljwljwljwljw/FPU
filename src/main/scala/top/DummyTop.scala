package top

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import fu._
import fu.divsqrt.DivSqrt
import fu.fma.FMA

class DummyTopIO extends Bundle{
  val in = Flipped(DecoupledIO(new Bundle() {
    val fu = UInt(3.W)
    val fuInput = new FPUSubModuleInput
  }))
  val out = DecoupledIO(new FPUSubModuleOutput)
}

class DummyTop extends Module {

  val io = IO(new DummyTopIO)

  val s_idle :: s_busy :: Nil = Enum(2)
  val state = RegInit(s_idle)
  switch(state){
    is(s_idle){
      when(io.in.valid){
        state := s_busy
      }
    }
    is(s_busy){
      when(io.out.fire()){
        state := s_idle
      }
    }
  }

  val subModules = Array[FPUSubModule](
    Module(new FMA),            // 0
    Module(new FCMP),           // 1
    Module(new FMV),            // 2
    Module(new FloatToInt),     // 3
    Module(new IntToFloat),     // 4
    Module(new F32toF64),       // 5
    Module(new F64toF32),       // 6
    Module(new DivSqrt)         // 7
  )

  val fuReg = RegEnable(io.in.bits.fu, io.in.fire())
  for((module, idx) <- subModules.zipWithIndex){
    module.io.in.bits := io.in.bits.fuInput
    module.io.in.valid := io.in.fire() && idx.U===io.in.bits.fu
    module.io.out.ready := io.out.ready
  }

  val subModuleOutput = Wire(Decoupled(new FPUSubModuleOutput))
  subModuleOutput := Mux1H(subModules.zipWithIndex.map({
    case (module, idx) =>
      (idx.U === fuReg) -> module.io.out
  }))

  io.in.ready := state === s_idle
  io.out <> subModuleOutput
}


object DummyTop extends App {
  (new chisel3.stage.ChiselStage).execute(
    args,
    Seq(ChiselGeneratorAnnotation(() => new DummyTop))
  )
}