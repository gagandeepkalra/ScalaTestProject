package coursera

object SquareRoot extends App {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) = (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  println(sqrt(16))
  println(sqrt(0.000001))
  println(sqrt(1e50))
  println(sqrt(1))
}
