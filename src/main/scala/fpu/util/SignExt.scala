package fpu.util

import chisel3._
import chisel3.util._

object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen == len) a else Cat(Fill(len - aLen, signBit), a)
  }
}
