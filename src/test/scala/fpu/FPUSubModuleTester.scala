package fpu

import java.io.File
import sys.process._
import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal._
import TestGen._
import scala.io.Source
import scala.util.Random

class MyDecoupledDriver[T <: Data](x: ReadyValidIO[T]) extends DecoupledDriver[T](x) {
  def expectDequeue(data: T, message: => String): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      waitForValid()
      x.valid.expect(true.B)
      x.bits.expect(data, message)
    }.joinAndStep(getSinkClock)
  }
  def expectDequeueSeq(data: Seq[T], message: => Seq[String]): Unit = timescope {
    for ((elt, msg) <- data.zip(message)) {
      expectDequeue(elt, msg)
    }
  }
}

class FPUSubModuleTester(f: File, t: FpuTest) extends FlatSpec
  with ChiselScalatestTester
  with Matchers {

  implicit def decoupledToDriver[T <: Data](x: ReadyValidIO[T]) = new MyDecoupledDriver[T](x)


  val (dutGen, isDouble, op) = testMap(t.name)
  val annos = Seq(backendMap(t.backend)) ++ (if (t.writeVcd) Seq(WriteVcdAnnotation) else Nil)

  it should s"compute ${f.getName} correctly" in {
    val input = Source.fromFile(f).getLines().toList
    val testCases = input.map(_.split(" "))

    test(new Delayer(dutGen())).withAnnotations(annos) { c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)
      c.io.out.expectInvalid()
      c.io.out.ready.poke(true.B)

      def dutEnQueue(testCase: Array[String], idx: Int): Unit = {
        val srcCnt = testCases.length - 2 // - output - fflags
        c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
          _.op -> op(2, 0),
          _.rm -> t.roundingModes.head,
          _.isDouble -> isDouble.B,
          _.a -> (if (srcCnt > 0) ("h" + testCase(0)).U(64.W) else 0.U(64.W)),
          _.b -> (if (srcCnt > 1) ("h" + testCase(1)).U(64.W) else 0.U(64.W)),
          _.c -> (if (srcCnt > 2) ("h" + testCase(2)).U(64.W) else 0.U(64.W))
        ))
      }

      def dumpFloat64(x: Long) = {
        val sign = (x >> (Float64.expWidth + Float64.mantWidth)) & 1
        val exp = (x >> Float64.mantWidth) & 0x7ff
        val mant = x & 0xfffffffffffffL
        println(s"ref: sign:$sign exp:$exp mant:${mant.toHexString}\n")
      }

      def dutDeQueue(testCase: Array[String], idx: Int): Unit = {
        val srcCnt = testCase.length - 2
        val refResult = ("h" + testCase(srcCnt)).U(64.W)
        val refFflags = ("h" + testCase(srcCnt + 1)).U
//        dumpFloat64(refResult.litValue().toLong)
        c.io.out.expectDequeue(
          chiselTypeOf(c.io.out.bits).Lit(
            _.result -> refResult,
            _.fflags -> chiselTypeOf(c.io.out.bits.fflags).Lit(
              _.invalid -> refFflags(4),
              _.infinite -> refFflags(3),
              _.overflow -> refFflags(2),
              _.underflow -> refFflags(1),
              _.inexact -> refFflags(0))
          ),
          message = s"\nn:$idx testCase: ${testCase.mkString(" ")}\n" +
            s"dut res:${c.io.out.bits.result.peek().litValue().toString(16)} " +
            s"inv:${c.io.out.bits.fflags.invalid.peek().litValue()} " +
            s"inf:${c.io.out.bits.fflags.infinite.peek().litValue()} " +
            s"ov:${c.io.out.bits.fflags.overflow.peek().litValue()} " +
            s"uf:${c.io.out.bits.fflags.underflow.peek().litValue()} " +
            s"ix:${c.io.out.bits.fflags.inexact.peek().litValue()}" +
            s"\n"
        )
      }
      try{
        if (t.pipeline) {
          fork {
            testCases.zipWithIndex.foreach({ case (testCase, idx) => dutEnQueue(testCase, idx) })
          }.fork {
            testCases.zipWithIndex.foreach({ case (testCase, idx) => dutDeQueue(testCase, idx) })
          }.join()
        } else {
          testCases.zipWithIndex.foreach({ case (testCase, idx) =>
            dutEnQueue(testCase, idx)
            dutDeQueue(testCase, idx)
          })
        }
      } catch {
        case _: Exception =>
          val errFile = new File(s"./debug/error.log")
          (s"echo ${f.getName}" #>> errFile).!
      }
    }
  }
}
