package fpu.util

import chisel3._

class Adder(len_a: Int, len_b: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(len_a.W))
    val b = Input(UInt(len_b.W))
    val out = Output(UInt((if(len_a > len_b) len_a else len_b).W))
  })
  io.out := io.a + io.b
}

object Adder {
  def apply(a: UInt, b: UInt): UInt = {
    val adder = Module(new Adder(a.getWidth, b.getWidth))
    adder.io.a := a
    adder.io.b := b
    adder.io.out
  }
}
