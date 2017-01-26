package uk.co.turingatemyhamster.numerics

object TestUnits {
  def main(args: Array[String]) = {

    import UnitPower.UnitOps
    import WithUnit.WithUnitOps

//    val btc = implicitly[ValueOf["BTC"]]
//    val one = implicitly[ValueOf[1]]

//    type oneBtc = "BTC" ^ 1
//    val oneBtc = valueOf[oneBtc]

//    type perLtc = "LTC" ^ (-1)
//    val perLtc = valueOf[perLtc]

//    type btcPerLtc = (oneBtc, perLtc)
//    val btcPerLtc = valueOf[btcPerLtc]

    val btc = "BTC" ^ 1
    val ltc = "LTC" ^ 1
    val perLtc = ltc.inverse
    val btcPerLtc = btc * perLtc
    val btcPerLtcLtc = btcPerLtc * ltc

    println(btc)
    println(ltc)
    println(perLtc)
    println(btcPerLtc)
    println(btcPerLtcLtc)

    val oneBtc = 1.0 ~* btc
    val twoBtcPerLtc = 2.0 ~* (btc / ltc)
    val threePerLtc = 3.0 ~/ ltc
    val fourLtc = 4.0 ~* ltc

    println(oneBtc)
    println(twoBtcPerLtc)
    println(threePerLtc)
    println(fourLtc)

    val onePerTwo = oneBtc / twoBtcPerLtc
    val someLtc = oneBtc * threePerLtc
    val twoThree = twoBtcPerLtc * threePerLtc
    val someBtc = twoBtcPerLtc * fourLtc

    println(onePerTwo)
    println(someLtc)
    println(twoThree)
    println(someBtc)
  }
}