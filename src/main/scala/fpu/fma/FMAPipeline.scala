package fpu.fma

import chisel3._
import chisel3.util._
import fpu._
import fpu.util.{CSA3_2, FPUDebug, ShiftLeftJam, ShiftRightJam}



class FMAExtOperands extends Module with HasFmaConst {
  val io = IO(new Bundle() {
    val in = new Bundle() {
      val isMul = Input(Bool())
      val isAddSub = Input(Bool())
      val isFma = Input(Bool())
      val isDouble = Input(Bool())
      val operands = Input(Vec(3, UInt(64.W)))
    }
    val out = new Bundle() {
      val operands = Output(Vec(3, UInt(64.W)))
    }
  })

  val isDouble = io.in.isDouble
  val (rs0, rs1, rs2) = (io.in.operands(0), io.in.operands(1), io.in.operands(2))
  val zero = 0.U(Float64.getWidth)
  val one = Mux(isDouble,
    Cat(0.U(1.W), Float64.expBiasInt.U(Float64.expWidth.W), 0.U(Float64.mantWidth.W)),
    Cat(0.U(1.W), Float32.expBiasInt.U(Float32.expWidth.W), 0.U(Float32.mantWidth.W))
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
  val a = Mux(io.in.isFma, rs2, Mux(io.in.isMul, zero, rs0)) //Mux(isFma, rs2, Mux(isMul, zero, rs1))
  val b = rs1
  val c = Mux(io.in.isAddSub, one, rs0)
  io.out.operands := VecInit(a, b, c).map(x => Mux(isDouble, x, extF32ToF64(x)))
}

class FMAPreNorm extends Module with HasFmaConst with HasUIntToSIntHelper {
  val io = IO(new Bundle() {
    val in = new Bundle() {
      val operands = Input(Vec(3, UInt(64.W)))
      val isSubnormal = Input(Vec(3, Bool()))
      val isZero = Input(Vec(3, Bool()))
    }
    val out = new Bundle() {
      val signs = Output(Vec(3, Bool()))
      val exps = Output(Vec(3, SInt((Float64.expWidth+1).W)))
      val mants = Output(Vec(3, UInt(D_MANT_WIDTH.W)))
    }
  })
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

  for(i <- 0 until 3){
    val (s, e, m) = decode(io.in.operands(i), io.in.isSubnormal(i), io.in.isZero(i))
    io.out.signs(i) := s
    io.out.exps(i) := e
    io.out.mants(i) := m
  }

}

abstract class FMAPath extends Module with HasUIntToSIntHelper with HasFmaConst {
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val doSub = Bool()
      val aIsZero = Bool()
      val prodIsZero = Bool()
      val aRealSign = Bool()
      val prodRealSign = Bool()
      val mantSum = UInt((ADD_WIDTH+1).W)
      val alignShiftAmt = UInt(log2Up(ADD_WIDTH).W)
      val exps = Vec(3, SInt((Float64.expWidth+1).W))
      val prodExpAdj = SInt(SEXP_WIDTH.W)
      val rm = UInt(3.W)
    })
    val out = Output(new Bundle() {
      val mant = UInt((D_MANT_WIDTH+3).W)
      val sign = Bool()
      val exp = SInt(SEXP_WIDTH.W)
    })
  })
  val doSub = io.in.doSub
  val aIsZero = io.in.aIsZero
  val prodIsZero = io.in.prodIsZero
  val aRealSign = io.in.aRealSign
  val prodRealSign = io.in.prodRealSign
  val mantSum = io.in.mantSum
  val alignShiftAmt = io.in.alignShiftAmt
  val exps = io.in.exps
  val rm = io.in.rm
  val prodExpAdj = io.in.prodExpAdj
}

class ClosePath extends FMAPath {

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

  io.out.sign := cpath_sign
  io.out.exp := cpath_exp
  io.out.mant := cpath_mant
}


class FarPath extends FMAPath {
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

  io.out.sign := fpath_sign
  io.out.exp := fpath_exp
  io.out.mant := fpath_mant
}

class FMAPipeline extends FPUSubModule with HasPipelineReg with HasUIntToSIntHelper with HasFmaConst {
  override def latency: Int = 4



  /*
        Stage 1
   */

  val in = io.in
  val (rs0, rs1, rs2) = (in.bits.a, in.bits.b, in.bits.c)
  val op = in.bits.op
  val isDouble = in.bits.isDouble
  val rm = in.bits.rm

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

  val FMAExtOperands = Module(new FMAExtOperands)
  FMAExtOperands.io.in.isMul := isMul
  FMAExtOperands.io.in.isAddSub := isAddSub
  FMAExtOperands.io.in.isFma := isFma
  FMAExtOperands.io.in.isDouble := isDouble
  FMAExtOperands.io.in.operands := VecInit(rs0, rs1, rs2)

  val operands = FMAExtOperands.io.out.operands
  val classify = Array.fill(3)(
    Module(new Classify(Float64.expWidth, Float64.mantWidth)).io
  )
  for((x, cls) <- operands.zip(classify)){
    cls.in := x
  }


  val FMAPreNorm = Module(new FMAPreNorm)
  FMAPreNorm.io.in.operands := operands
  FMAPreNorm.io.in.isSubnormal := VecInit(classify.map(_.isSubnormal))
  FMAPreNorm.io.in.isZero := VecInit(classify.map(_.isZero))

  val signs = FMAPreNorm.io.out.signs
  val exps = FMAPreNorm.io.out.exps
  val mants = FMAPreNorm.io.out.mants

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

  def dump(i: Int) = {
    printf(p"[$i] sign:${signs(i)} exp:${exps(i)}(${exps(i) - Float64.expBias.toSInt}) mant:${Hexadecimal(mants(i))}\n")
    printf(p"[$i] isSubmormal:${classify(i).isSubnormal} isNaN:${classify(i).isNaN} isInf:${classify(i).isInf}\n")
  }


  FPUDebug(valids(0)){
    for(i <- 0 until 3) dump(i)
    printf(p"special: ${specialCaseHappen} inv:${invalid} prodHasInf:${prodHasInf}\n")
  }

  val s1_exps = S1Reg(exps)
  val s1_mants = S1Reg(mants)
  val s1_prodIsZero = S1Reg(prodIsZero)
  val s1_aIsZero = S1Reg(aIsZero)
  val s1_doSub = S1Reg(doSub)
  val s1_aRealSign = S1Reg(aRealSign)
  val s1_rm = S1Reg(rm)
  val s1_prodRealSign = S1Reg(prodRealSign)
  val s1_isDobule = S1Reg(isDouble)
  val s1_specialCaseHappen = S1Reg(specialCaseHappen)
  val s1_specialOutput = S1Reg(specialOutput)
  val s1_invalid = S1Reg(invalid)



  /*
      Stage 2 -> Align A / mul part 1
   */

  val prodExp = s1_exps(1) +& s1_exps(2)
  val prodExpAdj = prodExp + 3.S
  val expDist = Wire(SInt(SEXP_WIDTH.W))
  expDist := prodExp - (s1_exps(0) +& (Float64.expBiasInt - INITIAL_EXP_DIFF).S)



  val discardProdMant = expDist < 0.S || s1_prodIsZero
  val discardAMant = !s1_prodIsZero && (expDist > (ADD_WIDTH-1).S || s1_aIsZero)
  val useClosePath = !discardAMant && (s1_prodIsZero || (expDist <= D_MANT_WIDTH.S))
  val (mantA, mantB, mantC)  = (s1_mants(0), s1_mants(1), s1_mants(2))
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
        !s1_aIsZero,
        false.B
      )
    ),
    (1 until D_MANT_WIDTH).map(
      i => (alignShiftAmt===(ADD_WIDTH-D_MANT_WIDTH+i).U) -> mantA(i-1, 0).orR()
    )
  )
  val alignedMantAPos = Cat(alignedMantARaw, alignSticky)
  val alignedMantAInv = (~alignedMantAPos).asUInt()
  val alignedMantA = Mux(s1_doSub, alignedMantAInv, alignedMantAPos)
  FPUDebug(valids(1)){
    printf(p"alighShiftAmt:${alignShiftAmt} discardMantA:${discardAMant} alignSticky:${alignSticky}\n")
  }
  val multiplier = Module(new ArrayMultiplier(D_MANT_WIDTH+1, hasReg = true, realArraryMult = true))
  multiplier.io.a := mantB
  multiplier.io.b := mantC
  multiplier.io.reg_en := valids(1) && rdyVec(1)

  val s2_prodIsZero = S2Reg(s1_prodIsZero)
  val s2_alignedMantA = S2Reg(alignedMantA)
  val s2_doSub = S2Reg(s1_doSub)
  val s2_aIsZero = S2Reg(s1_aIsZero)
  val s2_aRealSign = S2Reg(s1_aRealSign)
  val s2_prodRealSign = S2Reg(s1_prodRealSign)
  val s2_prodExpAdj = S2Reg(prodExpAdj)
  val s2_exps = S2Reg(s1_exps)
  val s2_alignShiftAmt = S2Reg(alignShiftAmt)
  val s2_rm = S2Reg(s1_rm)
  val s2_useCpath = S2Reg(useClosePath)
  val s2_isDobule = S2Reg(s1_isDobule)
  val s2_specialCaseHappen = S2Reg(s1_specialCaseHappen)
  val s2_specialOutput = S2Reg(s1_specialOutput)
  val s2_invalid = S2Reg(s1_invalid)

  /*
      Stage 3 product + addend
   */
  val prodSum = Mux(s2_prodIsZero, 0.U, multiplier.io.sum.tail(1))
  val prodCarry = Mux(s2_prodIsZero, 0.U, multiplier.io.carry.tail(1))
  FPUDebug(valids(2)){
    printf(p"alignedMantA:${Hexadecimal(s2_alignedMantA)}\n")
    printf(p"prod:${Hexadecimal(prodSum + prodCarry)}\n")
    printf(p"prodSum:${Hexadecimal(prodSum)} prodCarry:${Hexadecimal(prodCarry)}\n")
  }

  val mantAHighBits = s2_alignedMantA.head(D_MANT_WIDTH+2)
  val mantALowBits = s2_alignedMantA.tail(D_MANT_WIDTH+2).head(2*D_MANT_WIDTH)
  val mantAGRS = s2_alignedMantA(2, 0)

  val csa32 = Module(new CSA3_2(D_MANT_WIDTH * 2))
  csa32.io.in(0) := prodSum
  csa32.io.in(1) := prodCarry
  csa32.io.in(2) := mantALowBits

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

  val s3_doSub = S3Reg(s2_doSub)
  val s3_aIsZero = S3Reg(s2_aIsZero)
  val s3_prodIsZero = S3Reg(s2_prodIsZero)
  val s3_aRealSign = S3Reg(s2_aRealSign)
  val s3_prodRealSign = S3Reg(s2_prodRealSign)
  val s3_alignShiftAmt = S3Reg(s2_alignShiftAmt)
  val s3_exps = S3Reg(s2_exps)
  val s3_prodExpAdj = S3Reg(s2_prodExpAdj)
  val s3_rm = S3Reg(s2_rm)
  val s3_mantSum = S3Reg(mantSum)
  val s3_useCpath = S3Reg(s2_useCpath)
  val s3_isDobule = S3Reg(s2_isDobule)
  val s3_specialCaseHappen = S3Reg(s2_specialCaseHappen)
  val s3_specialOutput = S3Reg(s2_specialOutput)
  val s3_invalid = S3Reg(s2_invalid)

  /*
      Stage 4 -> cpath, fpath
   */
  val cpath = Module(new ClosePath)
  val fpath = Module(new FarPath)

  for(path <- Seq(cpath, fpath)){
    path.io.in.doSub := s3_doSub
    path.io.in.aIsZero := s3_aIsZero
    path.io.in.prodIsZero := s3_prodIsZero
    path.io.in.aRealSign := s3_aRealSign
    path.io.in.prodRealSign := s3_prodRealSign
    path.io.in.mantSum := s3_mantSum
    path.io.in.alignShiftAmt := s3_alignShiftAmt
    path.io.in.exps := s3_exps
    path.io.in.prodExpAdj := s3_prodExpAdj
    path.io.in.rm := s3_rm
  }

  FPUDebug(valids(3)){
    printf(p"mantSum:${Hexadecimal(s3_mantSum)} doSub:${s3_doSub}\n")
  }


  val s4_cpathResult = S4Reg(cpath.io.out)
  val s4_fpathResult = S4Reg(fpath.io.out)
  val s4_useCpath = S4Reg(s3_useCpath)
  val s4_rm = S4Reg(s3_rm)
  val s4_isDouble = S4Reg(s3_isDobule)
  val s4_prodIsZero = S4Reg(s3_prodIsZero)
  val s4_aIsZero = S4Reg(s3_aIsZero)
  val s4_specialCaseHappen = S4Reg(s3_specialCaseHappen)
  val s4_invalid = S4Reg(s3_invalid)
  val s4_specialOutput = S4Reg(s3_specialOutput)


  /*
      Stage 5
   */
  val common_sign = Mux(s4_useCpath, s4_cpathResult.sign, s4_fpathResult.sign)
  val common_exp = Mux(s4_useCpath, s4_cpathResult.exp, s4_fpathResult.exp)
  val denormShift = 1.S-common_exp
  val needDenormShift = denormShift > 0.S
  val common_mantNorm = Mux(s4_useCpath, s4_cpathResult.mant, s4_fpathResult.mant)
  val common_mantDeNorm = ShiftRightJam(common_mantNorm, denormShift.asUInt(), D_MANT_WIDTH+3)
  val common_mantUnrounded = Mux(needDenormShift, common_mantDeNorm, common_mantNorm)

  def mantWithGRStoP(x: UInt) = {
    p"mant:${Hexadecimal(x.head(D_MANT_WIDTH))} g:${x(2)} r:${x(1)} s:${x(0)}"
  }
  FPUDebug(valids.last){
    printf(mantWithGRStoP(common_mantNorm) + p"\n")
    printf(p" use cpath:${s4_useCpath}\n")
    printf(p"common exp:${common_exp}\n")
  }

  val rounding = Module(new RoundF64AndF32WithExceptions(expInHasBias = true))
  rounding.io.isDouble := s4_isDouble
  rounding.io.denormShiftAmt := denormShift
  rounding.io.sign := common_sign
  rounding.io.expNorm := common_exp
  rounding.io.mantWithGRS := common_mantUnrounded
  rounding.io.rm := s4_rm
  rounding.io.specialCaseHappen := s4_specialCaseHappen

  //val isZeroResult = rounding.io.isZeroResult
  val isZeroResult = s4_prodIsZero && s4_aIsZero
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

  val ovSetInf = s4_rm === RoudingMode.RNE ||
    s4_rm === RoudingMode.RMM ||
    (s4_rm === RoudingMode.RDN && common_sign) ||
    (s4_rm === RoudingMode.RUP && !common_sign)

  val result = Mux(s4_specialCaseHappen,
    s4_specialOutput,
    Mux(overflow,
      Cat(common_sign, Mux(ovSetInf, Float64.posInf, Float64.maxNorm).tail(1)),
      common_result
    )
  )

  FPUDebug(io.out.valid){
    printf(p"ov:${overflow} spec:${s4_specialCaseHappen} res:${Hexadecimal(result)}\n")
  }

  io.out.bits.result := result
  io.out.bits.fflags.invalid := s4_invalid
  io.out.bits.fflags.inexact := inexact
  io.out.bits.fflags.overflow := overflow
  io.out.bits.fflags.underflow := underflow
  io.out.bits.fflags.infinite := false.B

}
