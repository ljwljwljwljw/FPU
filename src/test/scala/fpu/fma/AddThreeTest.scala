package fpu.fma

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental.BundleLiterals._

import scala.util.Random

class AddThree extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(20.W))
    val b = Input(UInt(15.W))
    val c = Input(UInt(15.W))
    val result = Output(UInt(20.W))
  })

  // calculate |-a + (b + c) |

  val (a, b, c) = (Cat(0.U(1.W), io.a).asSInt(), io.b, io.c)
  val bAddC = Cat(0.U(1.W), io.b +& io.c).asSInt()
  val subRes = a - bAddC
  val abs = Mux(subRes > 0.S, subRes, bAddC - a)

  val aInv = (~io.a).asUInt()
  val sum = aInv +& bAddC.asUInt()

  io.result := abs.asUInt()
  printf(p"a: $a b: $b c: $c |-a+b+c|: ${abs}\n")

}

class AddThreeTest extends FlatSpec with ChiselScalatestTester with Matchers {

  it should "" in {
    test(new AddThree){ c =>
      for(i <- 0 until 100){
        val limit = 1 << 15
        val x = Random.nextInt(1 << 20)
        val y = Random.nextInt(limit)
        val z = Random.nextInt(limit)
        c.io.a.poke(x.U)
        c.io.b.poke(y.U)
        c.io.c.poke(z.U)
        c.clock.step(1)
        c.io.result.expect(math.abs(-x + y + z).U)
      }
    }
  }

}
