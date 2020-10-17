package fpu.util

import chisel3._
import chisel3.util._


class ShiftRightJam(x_width: Int, shiftAmt_width: Int, w: Int) extends Module {

  val xLen = if(x_width < w) w else x_width

  val io = IO(new Bundle() {
    val x = Input(UInt(x_width.W))
    val shiftAmt = Input(UInt(shiftAmt_width.W))
    val result = Output(UInt(w.W))
  })
  val x = io.x
  val shiftAmt = io.shiftAmt
  val result = io.result

  val x_ext = Wire(UInt(xLen.W))
  x_ext := (if(x.getWidth < w) Cat(x, 0.U((w-x.getWidth).W)) else x)
  val realShiftAmt = Mux(shiftAmt > (w-1).U,
    w.U,
    shiftAmt(log2Up(w) - 1, 0)
  )
  val mask = ((-1).S(xLen.W).asUInt() >> (w.U - realShiftAmt)).asUInt()
  val sticky = ORTree(mask & x_ext)
  val x_shifted = Wire(UInt(xLen.W))
  x_shifted := x_ext >> realShiftAmt
  io.result := x_shifted.head(w) | sticky
}

object ShiftRightJam {
  def apply(x: UInt, shiftAmt:UInt, w: Int): UInt ={
    val shiftRightJam = Module(new ShiftRightJam(x.getWidth, shiftAmt.getWidth, w))
    shiftRightJam.io.x := x
    shiftRightJam.io.shiftAmt := shiftAmt
    shiftRightJam.io.result
//    val xLen = if(x.getWidth < w) w else x.getWidth
//    val x_ext = Wire(UInt(xLen.W))
//    x_ext := (if(x.getWidth < w) Cat(x, 0.U((w-x.getWidth).W)) else x)
//    val realShiftAmt = Mux(shiftAmt > (w-1).U,
//      w.U,
//      shiftAmt(log2Up(w) - 1, 0)
//    )
//    val mask = ((-1).S(xLen.W).asUInt() >> (w.U - realShiftAmt)).asUInt()
//    val sticky = ORTree(mask & x_ext)
//    val x_shifted = Wire(UInt(xLen.W))
//    x_shifted := x_ext >> realShiftAmt
//    x_shifted.head(w) | sticky
  }
}
