package fpu.fma


import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class MulWrapper extends Module {
  val io = IO(new Bundle() {
    val a, b = Input(UInt(53.W))
    val success = Output(Bool())
  })
  val mul = Module(new ArrayMultiplier(53+1, 0, realArraryMult = true, hasReg = false))
  mul.io.a := io.a
  mul.io.b := io.b
  mul.io.reg_en := DontCare
  val ref = io.a * io.b
  val dut = mul.io.sum.tail(1) + mul.io.carry.tail(1)
  printf(p"ref:${Hexadecimal(ref)}\ndut:${Hexadecimal(dut)}\neq:${ref===dut} diff:${Hexadecimal(ref - dut)}\n\n")
  io.success := RegNext(next = ref===dut, init = true.B)
}


class MulTest extends FlatSpec with ChiselScalatestTester with Matchers {

  it should "" in {
    test(new MulWrapper){c =>
      for( i <- 0 until 1000) {
        val mask = 0x1fffffffffffffL
        val x = Random.nextLong() & mask
        val y = Random.nextLong() & mask
        c.io.a.poke(x.U)
        c.io.b.poke(y.U)
        c.io.success.expect(true.B)
        c.clock.step(1)
      }
    }
  }

}
