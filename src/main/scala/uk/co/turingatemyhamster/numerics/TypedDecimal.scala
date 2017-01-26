package uk.co.turingatemyhamster.numerics

import singleton.ops._

sealed trait DecimalPoint {
  p =>

  type places <: XInt
  def pow10: Long

  def suc[S <: XInt](implicit psuc: OpAuxInt[places + 1, S]): DecimalPoint.Aux[S] = new DecimalPoint {
    type places = S
    val pow10: Long = p.pow10 * 10l
  }
}

object DecimalPoint {
  type Aux[places0 <: XInt] = DecimalPoint { type places = places0 }

  implicit val point_0: DecimalPoint.Aux[0] = new DecimalPoint {
    type places = 0
    val pow10: Long = 1
  }

  implicit val point_1: DecimalPoint.Aux[1] = point_0.suc
  implicit val point_2: DecimalPoint.Aux[2] = point_1.suc
  implicit val point_3: DecimalPoint.Aux[3] = point_2.suc
  implicit val point_4: DecimalPoint.Aux[4] = point_3.suc
  implicit val point_5: DecimalPoint.Aux[5] = point_4.suc
  implicit val point_6: DecimalPoint.Aux[6] = point_5.suc
  implicit val point_7: DecimalPoint.Aux[7] = point_6.suc
  implicit val point_8: DecimalPoint.Aux[8] = point_7.suc
  implicit val point_9: DecimalPoint.Aux[9] = point_8.suc
  implicit val point_10: DecimalPoint.Aux[10] = point_9.suc
  implicit val point_11: DecimalPoint.Aux[11] = point_10.suc
  implicit val point_12: DecimalPoint.Aux[12] = point_11.suc
  implicit val point_13: DecimalPoint.Aux[13] = point_12.suc
  implicit val point_14: DecimalPoint.Aux[14] = point_13.suc
  implicit val point_15: DecimalPoint.Aux[15] = point_14.suc
  implicit val point_16: DecimalPoint.Aux[16] = point_15.suc
  implicit val point_17: DecimalPoint.Aux[17] = point_16.suc
  implicit val point_18: DecimalPoint.Aux[18] = point_17.suc
}

class TypedDecimal[P <: XInt](val digits: Long) extends AnyVal {
  lhs =>

  def + [Q <: XInt, R <: XInt, RP <: XInt, RQ <: XInt]
  (rhs: TypedDecimal[Q])
  (implicit
   max: OpAuxInt[Max[P, Q], R],
   rp: OpAuxInt[R - P, RP],
   rq: OpAuxInt[R - Q, RQ],
   rpd: DecimalPoint.Aux[RP],
   rqd: DecimalPoint.Aux[RQ]): TypedDecimal[R] =
    new TypedDecimal[R](lhs.digits * rpd.pow10 + rhs.digits * rqd.pow10)

  def - [Q <: XInt, R <: XInt, RP <: XInt, RQ <: XInt]
  (rhs: TypedDecimal[Q])
  (implicit
   max: OpAuxInt[Max[P, Q], R],
   rp: OpAuxInt[R - P, RP],
   rq: OpAuxInt[R - Q, RQ],
   rpd: DecimalPoint.Aux[RP],
   rqd: DecimalPoint.Aux[RQ]): TypedDecimal[R] =
    new TypedDecimal[R](lhs.digits * rpd.pow10 - rhs.digits * rqd.pow10)

  def * [Q <: XInt, R <: XInt]
  (rhs: TypedDecimal[Q])
  (implicit
   sum: OpAuxInt[P + Q, R]): TypedDecimal[R] =
    new TypedDecimal[R](lhs.digits * rhs.digits)

  def / [Q <: XInt, R <: XInt]
  (rhs: TypedDecimal[Q])
  (implicit
   diff: OpAuxInt[P - Q, R]): TypedDecimal[R] =
    new TypedDecimal[R](lhs.digits / rhs.digits)

  def ceil[Q <: XInt]
  (implicit rounding: RoundUp[P, Q]): TypedDecimal[Q] = rounding(lhs)

  def floor[Q <: XInt]
  (implicit rounding: RoundDown[P, Q]): TypedDecimal[Q] = rounding(lhs)

  def round[Q <: XInt]
  (implicit rounding: RoundToNearest[P, Q]): TypedDecimal[Q] = rounding(lhs)

  def prettyPrint(implicit pInt: ValueOf[P]): String = {
    val s = digits.toString
    val p = s.length - pInt.value
    s.substring(0, p) + "." + s.substring(p)
  }
}

object TypedDecimal {
  def apply[A, B](implicit b: TDBuilder[A, B]): TypedDecimal[b.L] = b.apply

  trait TDBuilder[A, B] {
    type L <: XInt
    def apply: TypedDecimal[L]
  }

  type Aux[A, B, L0 <: XInt] = TDBuilder[A, B] { type L = L0 }

  implicit def buildFromString[Lead <: XString, Trail <: XString, L0 <: XInt]
  (implicit len: OpAuxInt[Length[Trail], L0], dl: DecimalPoint.Aux[L0], lead: ValueOf[Lead], trail: ValueOf[Trail]): Aux[Lead, Trail, L0] = new TDBuilder[Lead, Trail] {
    type L = L0

    def apply = {
      val leadD = lead.value.toLong
      val trailD = trail.value.toLong

      new TypedDecimal[L](leadD * dl.pow10 + trailD)
    }
  }
}

sealed trait Rounding[P <: XInt, Q <: XInt] {
  def apply(p: TypedDecimal[P]): TypedDecimal[Q]
}

sealed trait RoundDown[P <: XInt, Q <: XInt] extends Rounding[P, Q]

object RoundDown {
  implicit def roundDownEq[P <: XInt]: RoundDown[P, P] = new RoundDown[P, P] {
    override def apply(p: TypedDecimal[P]): TypedDecimal[P] = p
  }

  implicit def roundDownLt[P <: XInt, Q <: XInt, R <: XInt]
  (implicit lt: P < Q, diff: OpAuxInt[Q - P, R], rd: DecimalPoint.Aux[R]): RoundDown[P, Q] = new RoundDown[P, Q] {
    override def apply(p: TypedDecimal[P]) = new TypedDecimal[Q](p.digits * rd.pow10)
  }

  implicit def roundDownGt[P <: XInt, Q <: XInt, R <: XInt]
  (implicit gt: P > Q, diff: OpAuxInt[P - Q, R], rd: DecimalPoint.Aux[R]): RoundDown[P, Q] = new RoundDown[P, Q] {
    override def apply(p: TypedDecimal[P]) = new TypedDecimal[Q](p.digits / rd.pow10)
  }
}

sealed trait RoundUp[P <: XInt, Q <: XInt] extends Rounding[P, Q]

object RoundUp {
  implicit def roundUpEq[P <: XInt]: RoundUp[P, P] = new RoundUp[P, P] {
    override def apply(p: TypedDecimal[P]): TypedDecimal[P] = p
  }

  implicit def roundUpLt[P <: XInt, Q <: XInt, R <: XInt]
  (implicit lt: P < Q, diff: OpAuxInt[Q - P, R], rd: DecimalPoint.Aux[R]): RoundUp[P, Q] = new RoundUp[P, Q] {
    override def apply(p: TypedDecimal[P]) = new TypedDecimal[Q](p.digits * rd.pow10)
  }

  implicit def roundUpGt[P <: XInt, Q <: XInt, R <: XInt, S <: XInt]
  (implicit gt: P > Q, diff: OpAuxInt[P - Q, R], rsup: OpAuxInt[R - 1, S], sd: DecimalPoint.Aux[S]): RoundUp[P, Q] = new RoundUp[P, Q] {
    override def apply(p: TypedDecimal[P]) = {
      val d10 = p.digits / sd.pow10
      val r = if(d10 % 10L == 0L) 0L else 1L
      new TypedDecimal[Q](d10 / 10L + r)
    }
  }
}

sealed trait RoundToNearest[P <: XInt, Q <: XInt] extends Rounding[P, Q]

object RoundToNearest {
  implicit def roundToNearestEq[P <: XInt]: RoundToNearest[P, P] = new RoundToNearest[P, P] {
    override def apply(p: TypedDecimal[P]): TypedDecimal[P] = p
  }

  implicit def roundToNearestLt[P <: XInt, Q <: XInt, R <: XInt]
  (implicit lt: P < Q, diff: OpAuxInt[Q - P, R], rd: DecimalPoint.Aux[R]): RoundToNearest[P, Q] = new RoundToNearest[P, Q] {
    override def apply(p: TypedDecimal[P]) = new TypedDecimal[Q](p.digits * rd.pow10)
  }

  implicit def roundToNearestGt[P <: XInt, Q <: XInt, R <: XInt, S <: XInt]
  (implicit gt: P > Q, diff: OpAuxInt[P - Q, R], rsup: OpAuxInt[R - 1, S], sd: DecimalPoint.Aux[S]): RoundToNearest[P, Q] = new RoundToNearest[P, Q] {
    override def apply(p: TypedDecimal[P]) = {
      val d10 = p.digits / sd.pow10
      val r = if(d10 % 10L < 5L) 0L else 1L
      new TypedDecimal[Q](d10 / 10L + r)
    }
  }
}
