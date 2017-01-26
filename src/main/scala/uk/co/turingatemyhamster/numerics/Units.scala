package uk.co.turingatemyhamster.numerics

//import singleton.ops._
//
//case class ^ [U <: XString, P <: XInt](unit: U, power: P) {
//  def inverse[I <: XInt](implicit negP: OpAuxInt[Negate[P], I], i: ValueOf[I]): U ^ I = ^(unit, i.value)
//}
//
//object ^ {
//  implicit def valueOfUnitPower[U <: XString, P <: XInt]
//  (implicit u: ValueOf[U], p: ValueOf[P]): ValueOf[U ^ P] = new ValueOf(^(u.value, p.value))
//}

case class UnitPower (unit: String, power: Int) {
  def inverse = UnitPower(unit, -power)
}

object UnitPower {

  implicit class UnitOps(val _s: String) extends AnyVal {
    def ^ (power: Int): UnitPower = UnitPower(_s, power)
  }

  implicit def toPowers(up: UnitPower): UnitPowers = UnitPowers(up :: Nil)
}

case class UnitPowers(ps: List[UnitPower]) {
  def * (ups: UnitPowers): UnitPowers = UnitPowers(UnitPowers.product(ps, ups.ps))
  def / (ups: UnitPowers): UnitPowers = *(ups.inverse)

  def inverse: UnitPowers = UnitPowers(ps map (_.inverse))
}

object UnitPowers {
  def productWithUnit(lhss: List[UnitPower], rhs: UnitPower): List[UnitPower] =
    lhss match {
      case lhs :: t if lhs.unit == rhs.unit =>
        lhs.power + rhs.power match {
          case 0 => t
          case p => UnitPower(lhs.unit, p) :: t
        }
      case h :: t =>
        h :: productWithUnit(t, rhs)
      case Nil =>
        rhs :: Nil
    }

  def product(lhss: List[UnitPower], rhss: List[UnitPower]): List[UnitPower] =
    rhss match {
      case h :: t =>
        product(productWithUnit(lhss, h), t)
      case Nil =>
        lhss
    }
}

case class WithUnit[V](value: V, units: UnitPowers) {
  lhs =>

  def * (rhs: WithUnit[V])(implicit V: Numeric[V]): WithUnit[V] =
    WithUnit(V.times(lhs.value, rhs.value), lhs.units * rhs.units)

  def / (rhs: WithUnit[V])(implicit V: Fractional[V]): WithUnit[V] =
    WithUnit(V.div(lhs.value, rhs.value), lhs.units / rhs.units)
}

object WithUnit {
  implicit class WithUnitOps[V](val _value: V) extends AnyVal {
    def ~* (ups: UnitPowers): WithUnit[V] = WithUnit(_value, ups)
    def ~/ (ups: UnitPowers): WithUnit[V] = WithUnit(_value, ups.inverse)
  }
}