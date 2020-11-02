package fpu

import chisel3._
import chisel3.util._


class FPUSubModuleInput extends Bundle{
  val op = UInt(3.W)
  val isDouble = Bool()
  val a, b, c = UInt(64.W)
  val rm = UInt(3.W)
}

class FPUSubModuleOutput extends Bundle{
  val fflags = new Fflags
  val result = UInt(64.W)
}

class FPUSubModuleIO extends Bundle{
  val in = Flipped(DecoupledIO(new FPUSubModuleInput))
  val out = DecoupledIO(new FPUSubModuleOutput)
}

trait HasPipelineReg { this: FPUSubModule =>
  def latency: Int


  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val valids = validVec
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  for(i <- 0 until latency){
    rdyVec(i) := !validVec(i+1) || rdyVec(i+1)
  }
  for(i <- 1 to latency){
    when(rdyVec(i) && !validVec(i-1)){
      validVec(i) := false.B
    }.elsewhen(rdyVec(i-1) && validVec(i-1)){
      validVec(i) := validVec(i-1)
    }
  }

  def PipelineReg[T<:Data](i: Int)(next: T) = RegEnable(next, enable = valids(i-1) && rdyVec(i-1))
  def S1Reg[T<:Data](next: T):T = PipelineReg[T](1)(next)
  def S2Reg[T<:Data](next: T):T = PipelineReg[T](2)(next)
  def S3Reg[T<:Data](next: T):T = PipelineReg[T](3)(next)
  def S4Reg[T<:Data](next: T):T = PipelineReg[T](4)(next)
  def S5Reg[T<:Data](next: T):T = PipelineReg[T](5)(next)

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last

  //printf(p"valids:${Binary(Cat(valids))} ${Binary(Cat(validVec))} rdy:${Binary(Cat(rdyVec))} valids0:${valids(0)}\n")

}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

abstract class FPUSubModule extends Module with HasUIntToSIntHelper {
  val io = IO(new FPUSubModuleIO)
}
