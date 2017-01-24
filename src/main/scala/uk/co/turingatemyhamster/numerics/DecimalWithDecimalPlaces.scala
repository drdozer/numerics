package uk.co.turingatemyhamster.numerics


case class DecimalWithDecimalPlaces(digits: Long, decimalPlaces: Byte) {
  def extendTo(decimalPlaces: Byte): DecimalWithDecimalPlaces = {
    var ps = this.decimalPlaces - decimalPlaces
    var d = digits
    
    while(ps < 0) {
      d *= 10
      ps += 1
    }
      
    while(ps > 0) {
      d /= 10
      ps -= 1
    }  
    
    DecimalWithDecimalPlaces(d, decimalPlaces)
  }
  
  def prettyPrint: String = {
    val s = digits.toString
    val p = s.length - decimalPlaces
    s.substring(0, p) + "." + s.substring(p)
  }
}

object DecimalWithDecimalPlaces {
  implicit object dwdpNumeric extends Numeric[DecimalWithDecimalPlaces] {
    override def compare(x: DecimalWithDecimalPlaces, y: DecimalWithDecimalPlaces) = minus(y, x).toInt

    override def fromInt(x: Int) = DecimalWithDecimalPlaces(x.toLong, 0.toByte)

    override def minus(x: DecimalWithDecimalPlaces, y: DecimalWithDecimalPlaces) = {
      val decimalPlaces = Math.max(x.decimalPlaces.toInt, y.decimalPlaces.toInt).toByte
      DecimalWithDecimalPlaces(x.extendTo(decimalPlaces).digits - y.extendTo(decimalPlaces).digits, decimalPlaces)
    }

    override def negate(x: DecimalWithDecimalPlaces) = DecimalWithDecimalPlaces(-x.digits, x.decimalPlaces)

    override def plus(x: DecimalWithDecimalPlaces, y: DecimalWithDecimalPlaces) = {
      val decimalPlaces = Math.max(x.decimalPlaces.toInt, y.decimalPlaces.toInt).toByte
      DecimalWithDecimalPlaces(x.extendTo(decimalPlaces).digits + y.extendTo(decimalPlaces).digits, decimalPlaces.toByte)
    }

    override def times(x: DecimalWithDecimalPlaces, y: DecimalWithDecimalPlaces) = {
      val decimalPlaces = (x.decimalPlaces + y.decimalPlaces).toByte
      DecimalWithDecimalPlaces(x.digits * y.digits, decimalPlaces.toByte)
    }

    override def toDouble(x: DecimalWithDecimalPlaces) = x.digits.toDouble / Math.pow(10.0, x.decimalPlaces.toDouble)

    override def toFloat(x: DecimalWithDecimalPlaces) = toDouble(x).toFloat

    override def toInt(x: DecimalWithDecimalPlaces) = toDouble(x).toInt

    override def toLong(x: DecimalWithDecimalPlaces) = toDouble(x).toLong
  }
}

