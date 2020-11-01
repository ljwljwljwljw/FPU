package fpu.fma

import chisel3._
import chisel3.util._
import fpu._
import fpu.util._

trait HasFmaConst {
  def SEXP_WIDTH: Int = Float64.expWidth + 2
  def D_MANT_WIDTH: Int = Float64.mantWidth + 1
  def S_MANT_WIDTH: Int = Float32.mantWidth + 1
  def INITIAL_EXP_DIFF: Int = Float64.mantWidth + 4
  def ADD_WIDTH: Int = D_MANT_WIDTH + 2 + 2*D_MANT_WIDTH + 2
}

class FMANew extends FPUSubModule with HasFmaConst {

  val in = io.in
  val (rs0, rs1, rs2) = (in.bits.a, in.bits.b, in.bits.c)
  val op = in.bits.op
  val isDouble = in.bits.isDouble
  val rm = in.bits.rm
  val zero = 0.U(Float64.getWidth)
  val one = Mux(isDouble,
    Cat(0.U(1.W), Float64.expBiasInt.U(Float64.expWidth.W), 0.U(Float64.mantWidth.W)),
    Cat(0.U(1.W), Float32.expBiasInt.U(Float32.expWidth.W), 0.U(Float32.mantWidth.W))
  )

  def isOneOf(x: UInt, seq: Seq[UInt]): Bool = {
    if(seq.isEmpty) false.B
    else Cat(seq.map(s => x===s)).orR()
  }

  val isMul = op==="b010".U
  val isAddSub = op(2, 1)===0.U
  val isFma = op(2)
  val negA = op(0) & op(2) // fmsub or fnmadd
  val negProd = isOneOf(op,
    Seq(FPUOpType.fsub, FPUOpType.fnmsub, FPUOpType.fnmadd).map(_(2, 0))
  )

  /*
            fadd   ->  a + b * 1
            fsub   ->  a - b * 1
            fmul   ->  0 + b * c
            fmadd  ->  a + b * c
            fmsub  -> -a + b * c
            fnmsub ->  a - b * c
            fnmadd -> -a - b * c
  */

  val a = Mux(isFma, rs2, Mux(isMul, zero, rs0)) //Mux(isFma, rs2, Mux(isMul, zero, rs1))
  val b = rs1
  val c = Mux(isAddSub, one, rs0)
  FPUDebug(){
    printf(p"A:${H(a)} B:${H(b)} C:${H(c)} rm=${rm}\n")
  }
  val operands = Seq(a, b, c).map(x => Mux(isDouble, x, extF32ToF64(x)))
  val classify = Array.fill(3)(
    Module(new Classify(Float64.expWidth, Float64.mantWidth)).io
  )
  for((x, cls) <- operands.zip(classify)){
    cls.in := x
  }

  // return [sign, exp (with bias), mant(with hidden bit)]
  def decode(x: UInt, isSubnormal: Bool, isZero: Bool): (Bool, SInt, UInt) = {
    val f64 = Float64(x)
    val mantLez = PriorityEncoder(Cat(0.U(1.W), f64.mant).asBools().reverse)
    val mantAdj = Mux(isSubnormal,
      (f64.mant << mantLez)(D_MANT_WIDTH-1, 0),
      Cat(1.U(1.W), f64.mant)
    )
//    printf(p"decode: lez:${mantLez} mantAdj:${mantAdj} raw:${H(f64.mant)} shift:${H(f64.mant << mantLez)}\n")
    val mantExt = Mux(isZero, 0.U, mantAdj)
    val exp = Mux(isSubnormal, 1.S - mantLez.toSInt, f64.exp.toSInt)
    (f64.sign, exp, mantExt)
  }

  val signs = Array.fill(3)(Wire(Bool()))
  val exps = Array.fill(3)(Wire(SInt((Float64.expWidth+1).W)))
  val mants = Array.fill(3)(Wire(UInt(D_MANT_WIDTH.W)))
  for(i <- 0 until 3){
    val (s, e, m) = decode(operands(i), classify(i).isSubnormal, classify(i).isZero)
    signs(i) := s
    exps(i) := e
    mants(i) := m
    FPUDebug(){
      dump(i)
    }
  }

  def H(x: Bits) = Hexadecimal.apply(x)
  def dump(i: Int) = {
    printf(p"[$i] sign:${signs(i)} exp:${exps(i)}(${exps(i) - Float64.expBias.toSInt}) mant:${H(mants(i))}\n")
    printf(p"[$i] isSubmormal:${classify(i).isSubnormal} isNaN:${classify(i).isNaN} isInf:${classify(i).isInf}\n")
  }


  val prodSign = signs(1) ^ signs(2)
  val aSign = Mux(isMul, prodSign, signs.head)
  val prodRealSign = prodSign ^ isOneOf(op, Seq(FPUOpType.fsub, FPUOpType.fnmsub, FPUOpType.fnmadd).map(_(2, 0)))
  val aRealSign = aSign ^ isOneOf(op, Seq(FPUOpType.fmsub, FPUOpType.fnmadd).map(_(2, 0)))
  val doSub = aRealSign ^ prodRealSign

  val hasNaN = classify.map(_.isNaN).reduce(_||_)
  val hasSNaN = classify.map(_.isSNaN).reduce(_||_)
  val prodHasNaN = classify.drop(1).map(_.isNaN).reduce(_||_)

  val isInf = classify.map(_.isInf)
  val aIsInf = isInf(0)
  val prodHasInf = isInf.drop(1).reduce(_||_)
  val hasInf = isInf(0) || prodHasInf

  val aIsZero = classify(0).isZero
  val prodIsZero = classify.drop(1).map(_.isZero).reduce(_||_)
  val addInfInvalid = !prodHasNaN && (aIsInf && prodHasInf && doSub)
  val zeroMulInf = prodIsZero && prodHasInf

  val infInvalid = addInfInvalid || zeroMulInf

  val invalid = hasSNaN || infInvalid
  val specialCaseHappen = hasNaN || hasInf
  val specialOutput = PriorityMux(Seq(
    (hasNaN || infInvalid) -> Mux(isDouble,
      Float64.defaultNaN,
      Float32.defaultNaN
    ),
    aIsInf -> Mux(isDouble,
      Cat(aRealSign, Float64.posInf.tail(1)),
      Cat(aRealSign, Float32.posInf.tail(1))
    ),
    prodHasInf -> Mux(isDouble,
      Cat(prodRealSign, Float64.posInf.tail(1)),
      Cat(prodRealSign, Float32.posInf.tail(1))
    )
  ))




  /*
      Align A
   */
  val prodExp = exps(1) +& exps(2)
  val prodExpAdj = prodExp + 3.S
  val expDist = Wire(SInt(SEXP_WIDTH.W))
  expDist := prodExp - (exps(0) +& (Float64.expBiasInt - INITIAL_EXP_DIFF).S)

  val discardProdMant = expDist < 0.S || prodIsZero
  val discardAMant = !prodIsZero && (expDist > (ADD_WIDTH-1).S || aIsZero)
  val useClosePath = !discardAMant && (prodIsZero || (expDist <= D_MANT_WIDTH.S))
  val (mantA, mantB, mantC)  = (mants(0), mants(1), mants(2))
  val mantAPaddingZeros = Cat(mantA, 0.U((ADD_WIDTH - D_MANT_WIDTH).W))
  val alignShiftAmt = Mux(discardProdMant || discardAMant,
    0.U,
    expDist.asUInt()(log2Up(ADD_WIDTH)-1, 0)
  )
  val alignedMantARaw = Mux(discardAMant,
    0.U,
    (mantAPaddingZeros >> alignShiftAmt).asUInt()
  )
  assert(alignedMantARaw.getWidth == ADD_WIDTH)
  val alignSticky = MuxCase(
    Mux(discardProdMant,
      false.B,
      Mux(discardAMant,
        !aIsZero,
        false.B
      )
    ),
    (1 until D_MANT_WIDTH).map(
      i => (alignShiftAmt===(ADD_WIDTH-D_MANT_WIDTH+i).U) -> mantA(i-1, 0).orR()
    )
  )
  val alignedMantAPos = Cat(alignedMantARaw, alignSticky)
  val alignedMantAInv = (~alignedMantAPos).asUInt()
  val alignedMantA = Mux(doSub, alignedMantAInv, alignedMantAPos)


  FPUDebug(){
    printf(p"doSub:${doSub} expDiff:${expDist} alignShiftAmt:${alignShiftAmt} cpath:${useClosePath} special:${specialCaseHappen}\n")
    printf(p"discardMantA:${discardAMant} discardProdMant:${discardProdMant} alignSticky:${alignSticky}\n")
  }

  /*
      Calculate product
   */
  val multiplier = Module(new ArrayMultiplier(D_MANT_WIDTH+1, hasReg = false, realArraryMult = true))
  multiplier.io.a := mantB
  multiplier.io.b := mantC
  multiplier.io.reg_en := DontCare

  val prodSum = Mux(prodIsZero, 0.U, multiplier.io.sum.tail(1))
  val prodCarry = Mux(prodIsZero, 0.U, multiplier.io.carry.tail(1))
  FPUDebug(){
    printf(p"prod:${Hexadecimal(prodSum + prodCarry)}\n")
  }

  /*
      AlignedMantA + prodSum + prodCarry
   */
  val mantAHighBits = alignedMantA.head(D_MANT_WIDTH+2)
  val mantALowBits = alignedMantA.tail(D_MANT_WIDTH+2).head(2*D_MANT_WIDTH)
  val mantAGRS = alignedMantA(2, 0)

  val csa32 = Module(new CSA3_2(D_MANT_WIDTH * 2))
  csa32.io.in(0) := prodSum
  csa32.io.in(1) := prodCarry
  csa32.io.in(2) := mantALowBits
  FPUDebug(){
    printf(p"prodSum:${H(prodSum)} prodCarry:${H(prodCarry)}\n")
  }

  val csaSum = csa32.io.out(0) + Cat(csa32.io.out(1), 0.U(1.W))

  val mantSumHigh = Mux(csaSum.head(1).asBool(),
    mantAHighBits + 1.U,
    mantAHighBits
  )

  val mantSum = Cat(
    mantSumHigh,
    csaSum.tail(1),
    mantAGRS
  )

  FPUDebug(){
    printf(p"mantSum:${H(mantSum)}\n")
  }

  /*
      1. Close Path
        Leading zeros <- [0, MANT_WIDTH+1]
   */
  val cpath_mantLen = D_MANT_WIDTH+1+D_MANT_WIDTH+2
  val cpath_mantAbs = Mux(doSub,
    ~mantSum.head(cpath_mantLen),
    mantSum.head(cpath_mantLen)
  )
  val cpath_addSticky = mantSum.tail(cpath_mantLen).orR()
  val cpath_subSticky = !mantSum.tail(cpath_mantLen).andR()
  val cpath_mantSticky = Mux(doSub,
    cpath_subSticky,
    cpath_addSticky
  )
  val cpath_mantPreNorm = Cat(cpath_mantAbs, cpath_mantSticky)
  val cpath_mantPreNormLez = PriorityEncoder(cpath_mantPreNorm.asBools().reverse)
  // left shift
  val cpath_mantPostNorm = (cpath_mantPreNorm << alignShiftAmt)(cpath_mantLen+1, 0)
  val cpath_maxShift = Cat(cpath_mantPostNorm(cpath_mantLen-1, 0), 0.U(1.W))
  val cpath_midShift = cpath_mantPostNorm(cpath_mantLen, 0)
  val cpath_minShift = cpath_mantPostNorm(cpath_mantLen+1, 1)
  val cpath_mantShift = Mux(cpath_minShift.head(1).asBool(),
    cpath_minShift,
    Mux(cpath_midShift.head(1).asBool(),
      cpath_midShift,
      cpath_maxShift
    )
  )
  val cpath_expAdj = Mux(cpath_minShift.head(1).asBool(),
    1.S,
    Mux(cpath_midShift.head(1).asBool(),
      0.S,
      (-1).S
    )
  )
  val cpath_mant = Cat(
    cpath_mantShift.head(D_MANT_WIDTH+2),
    cpath_mantShift.tail(D_MANT_WIDTH+2).orR()
  )
  val cpath_isZero = aIsZero && prodIsZero
  val cpath_sign = Mux(cpath_isZero,
    (aRealSign & prodRealSign) | ((aRealSign | prodRealSign) & rm===RoudingMode.RDN),
    aRealSign
  )
  val cpath_exp = exps(0) + cpath_expAdj

  FPUDebug(){
    printf(p"cpath: sticky:$cpath_mantSticky mantAbs:${H(cpath_mantAbs)}\n")
    printf(p"cpath: lez:${cpath_mantPreNormLez} expAdj:${cpath_expAdj}\n")
    printf(p"cpath: sign:$cpath_sign exp:$cpath_exp " + mantWithGRStoP(cpath_mant) + "\n")
  }

  /*
      2. Far Path
   */
  val fpath_mantSumSign = mantSum(2*D_MANT_WIDTH+5)
  val fpath_mantAbs = Mux(fpath_mantSumSign,
    (~mantSum(2*D_MANT_WIDTH+4, 0)).asUInt(),
    mantSum(2*D_MANT_WIDTH+4, 0) + doSub
  )
  val fpath_lez = PriorityEncoder(fpath_mantAbs.asBools().reverse)
  val fpath_mant = ShiftLeftJam(fpath_mantAbs, fpath_lez, D_MANT_WIDTH+3)
  val fpath_isZero = !fpath_mant.head(1).asBool()
  val fpath_sign = Mux(fpath_isZero,
    rm === RoudingMode.RDN,
    prodRealSign ^ fpath_mantSumSign
  )
  val fpath_exp = prodExpAdj - fpath_lez.toSInt - Float64.expBias.toSInt

  FPUDebug(){
    printf(p"fpath: lez:${fpath_lez} mantAbs:${H(fpath_mantAbs)} prodRealSign:${prodRealSign} fmantSumSign:${fpath_mantSumSign}\n")
  }

  def mantWithGRStoP(x: UInt) = {
    p"mant:${H(x.head(D_MANT_WIDTH))} g:${x(2)} r:${x(1)} s:${x(0)}"
  }

  FPUDebug(){
    printf(p"fpath: sign:$fpath_sign exp:$fpath_exp " + mantWithGRStoP(fpath_mant) + "\n")
  }

  /*
      Rounding
   */


  val common_sign = Mux(useClosePath, cpath_sign, fpath_sign)
  val common_exp = Mux(useClosePath, cpath_exp, fpath_exp)

  val denormShift = 1.S-common_exp
  val needDenormShift = denormShift > 0.S
  val common_mantNorm = Mux(useClosePath, cpath_mant, fpath_mant)
  val common_mantDeNorm = ShiftRightJam(common_mantNorm, denormShift.asUInt(), D_MANT_WIDTH+3)
  val common_mantUnrounded = Mux(needDenormShift, common_mantDeNorm, common_mantNorm)
  FPUDebug(){
    printf(p"denormShift:${denormShift}\n")
    printf(p"common: exp:${common_exp}\n")
  }

  val rounding = Module(new RoundF64AndF32WithExceptions(expInHasBias = true))
  rounding.io.isDouble := isDouble
  rounding.io.denormShiftAmt := denormShift
  rounding.io.sign := common_sign
  rounding.io.expNorm := common_exp
  rounding.io.mantWithGRS := common_mantUnrounded
  rounding.io.rm := rm
  rounding.io.specialCaseHappen := specialCaseHappen

  //val isZeroResult = rounding.io.isZeroResult
  val isZeroResult = prodIsZero && aIsZero
  val expRounded = rounding.io.expRounded
  val mantRounded = rounding.io.mantRounded
  val overflow = rounding.io.overflow
  val underflow = rounding.io.underflow
  val inexact = rounding.io.inexact

  val common_result = Cat(
    common_sign,
    expRounded(Float64.expWidth-1, 0),
    mantRounded(Float64.mantWidth-1, 0)
  )
  FPUDebug(){
    printf(p"after round: sign:${common_sign} " +
      p"exp:${expRounded(Float64.expWidth-1, 0)} mant:${H(mantRounded(Float64.mantWidth-1, 0))}\n")
  }

  val ovSetInf = rm === RoudingMode.RNE ||
    rm === RoudingMode.RMM ||
    (rm === RoudingMode.RDN && common_sign) ||
    (rm === RoudingMode.RUP && !common_sign)

  val result = Mux(specialCaseHappen,
    specialOutput,
    Mux(overflow,
      Cat(common_sign, Mux(ovSetInf, Float64.posInf, Float64.maxNorm).tail(1)),
      common_result
    )
  )

  io.out.bits.result := result
  io.out.bits.fflags.invalid := invalid
  io.out.bits.fflags.inexact := inexact
  io.out.bits.fflags.overflow := overflow
  io.out.bits.fflags.underflow := underflow
  io.out.bits.fflags.infinite := false.B

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

}
