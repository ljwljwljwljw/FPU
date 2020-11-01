package fpu.util

import chisel3._

object FPUDebug {
  var flag = false
  // don't care GTimer in FPU tests
  def apply(cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond) { body } }
}
