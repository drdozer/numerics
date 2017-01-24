package uk.co.turingatemyhamster.numerics

/**
  * Created by nmrp3 on 23/01/17.
  */
object TestMe {
  def main(args: Array[String]) = {
    val pi2 = new TypedDecimal[2](314)
    println(s"pi2: $pi2 ${pi2.digits} ${pi2.prettyPrint}")

    val pi2_2: TypedDecimal[2] = pi2 + pi2
    println(s"pi2_2: $pi2_2 ${pi2_2.digits} ${pi2_2.prettyPrint}")

    val pi10 = new TypedDecimal[1](314)
    println(s"pi10: $pi10 ${pi10.digits} ${pi10.prettyPrint}")

    val pi102: TypedDecimal[2] = pi10 + pi2
    println(s"pi102: $pi102 ${pi102.digits} ${pi102.prettyPrint}")

    val pi11: TypedDecimal[5] = pi2 * new TypedDecimal[3](1100)
    println(s"pi11: $pi11 ${pi11.digits} ${pi11.prettyPrint}")


    implicit def foo[A: ValueOf, B: ValueOf]: ValueOf[(A, B)] = new ValueOf((valueOf[A], valueOf[B]))

    val btc = implicitly[ValueOf["BTC"]]
    val one = implicitly[ValueOf[1]]
    val oneBtc = implicitly[ValueOf[("BTC", 1)]]
  }
}
