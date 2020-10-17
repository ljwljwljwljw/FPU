package fpu.util

import chisel3._
import chisel3.util._

class ShiftLeftJam(x_width: Int, shiftAmt_width: Int, w: Int) extends Module {
  val xLen = if(x_width < w) w else x_width
  val io = IO(new Bundle() {
    val x = Input(UInt(x_width.W))
    val shiftAmt = Input(UInt(shiftAmt_width.W))
    val result = Output(UInt(w.W))
  })
  val x = io.x
  val shiftAmt = io.shiftAmt
  val x_shifted = Wire(UInt(xLen.W))
  x_shifted := Mux(shiftAmt > (xLen-1).U,
    0.U,
    x << shiftAmt(log2Up(xLen)-1, 0)
  )
  val sticky = ORTree(x_shifted.tail(w))
  io.result := x_shifted.head(w) | sticky
}

object ShiftLeftJam {
  def apply(x: UInt, shiftAmt: UInt, w:Int): UInt = {
    val shiftLeftJam = Module(new ShiftLeftJam(x.getWidth, shiftAmt.getWidth, w))
    shiftLeftJam.io.x := x
    shiftLeftJam.io.shiftAmt := shiftAmt
    shiftLeftJam.io.result
//    val xLen = if(x.getWidth < w) w else x.getWidth
//    val x_shifted = Wire(UInt(xLen.W))
//    x_shifted := Mux(shiftAmt > (xLen-1).U,
//      0.U,
//      x << shiftAmt(log2Up(xLen)-1, 0)
//    )
//    val sticky = ORTree(x_shifted.tail(w))
//    x_shifted.head(w) | sticky
  }
}
