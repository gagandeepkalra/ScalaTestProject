object CaptainPrime {

  val MAX = 1000001
  val factors = new Array[Int](MAX)

  (2 to MAX - 1).foreach { i =>
    if (factors(i) == 0) {
      (2 * i to MAX - 1 by i).foreach { j =>
        factors(j) = i
      }
    }
  }

  factors(1) = 1

  def isPrime(n: Int) = factors(n) == 0

  def containsZeroAsDigit(n: Int): Boolean = n.toString.contains("0")

  def leftDigitsTakenOffRestStillPrime(n: Int): Boolean = {
    var str = n.toString
    var result = true
    for (i <- 1 to str.length - 1) {
      if (!isPrime(str.substring(i).toInt)) result = false
    }
    result
  }

  def rightDigitsTakenOffRestStillPrime(n: Int): Boolean = {
    var x = n
    while (x != 0) {
      x = x / 10
      if (!isPrime(x)) return false
    }
    true
  }

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt()).foreach(_ => {
      val n = io.StdIn.readInt()

      if (isPrime(n) && !containsZeroAsDigit(n)) {
        val leftPrime = leftDigitsTakenOffRestStillPrime(n)
        val rightPrime = rightDigitsTakenOffRestStillPrime(n)

        if (leftPrime && rightPrime) println("CENTRAL")
        else if (leftPrime) println("LEFT")
        else if (rightPrime) println("RIGHT")
        else println("DEAD")

      } else println("DEAD")

    })
  }

}
