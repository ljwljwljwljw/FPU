package fpu.fma

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental.BundleLiterals._

import scala.util.Random

class Add extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(20.W))
    val b = Input(UInt(20.W))
    val success = Output(Bool())
  })

  val (a, b) = (io.a, io.b)
  // a - b
  val golden_abs = Mux(a > b, a - b, b - a)
  val golden_sign = Mux(a >= b, false.B, true.B)

  val b_inv = (~b).asUInt()
  val addRes = a +& b_inv
  val cout = addRes.head(1).asBool()
  val r = addRes.tail(1)

  val abs = Mux(cout, r + 1.U, ~r).asUInt()
  val a_sign = a.head(1).asBool()

  io.success := RegNext((abs===golden_abs) && ((!cout)===golden_sign), true.B)

  printf(p"a=$a b=$b golden:$golden_abs act:${abs.asUInt()} cout:${cout}\n")
}

class AddTest extends FlatSpec with ChiselScalatestTester with Matchers {

  case class X() {
    var a = 0
  }

  it should "" in {
    test(new Add){ c =>
      for(i <- 0 until 100){
        val limit = 1 << 20
        val x = Random.nextInt(limit)
        val y = Random.nextInt(limit)
        c.io.a.poke(x.U)
        c.io.b.poke(y.U)
        c.io.success.expect(true.B)
        c.clock.step(1)
      }
    }
  }

}
