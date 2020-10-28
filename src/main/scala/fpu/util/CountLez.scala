package fpu.util

import chisel3._
import chisel3.util._

class CountLez(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(UInt(len.W))
    val out = Output(UInt(log2Up(len).W))
  })
  io.out := PriorityEncoder(io.in)
}

object CountLez {
  def apply(x: UInt): UInt= {
    val countLez = Module(new CountLez(x.getWidth))
    countLez.io.in := x
    countLez.io.out
  }
}
