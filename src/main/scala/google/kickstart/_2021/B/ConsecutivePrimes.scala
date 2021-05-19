package google.kickstart._2021.B

/**
  * https://codingcompetitions.withgoogle.com/kickstart/round/0000000000435a5b/000000000077a8e6#problem
  *
  * The secret code is a number that can be formed by taking the product of two consecutive prime numbers, such that it
  * is the largest number that is smaller than or equal to Z. Given the value of Z, help John to determine the secret code.
  *
  * [Sieve of Eratosthenes, Segment Sieve]
  */
object ConsecutivePrimes {

  val Max = 100001

  val nthPrime: IndexedSeq[Int] = {
    val isPrime = Array.fill(Max)(true)
    isPrime(0) = false
    isPrime(1) = false

    for {
      i <- 2 until Max if isPrime(i)
      j <- i * 2 until Max by i
    } isPrime(j) = false

    (0 until Max).collect { case i if isPrime(i) => i }
  }
  val isPrime: Set[Int] = nthPrime.toSet

  def primalStream(stream: Stream[Long]): Stream[Long] = {
    val SegmentLength = 1000

    def collectPrimes(stream: Stream[Long]): Stream[Long] = {
      val (subStream, remaining) = stream.splitAt(SegmentLength)

      if (subStream.isEmpty) Stream.Empty
      else {
        val (l, r) = {
          val a = subStream.head
          val b = subStream.last

          if (a <= b) (a, b) else (b, a)
        }

        {
          if (r < Max) subStream.collect { case i if isPrime(i.toInt) => i } else {
            val subStreamL = subStream.length
            val sqrtR      = math.sqrt(r).ceil.toInt
            val isPrime    = Array.fill(subStreamL)(true)

            @scala.annotation.tailrec
            def loop(pIdx: Int): Unit = {
              if (pIdx < nthPrime.length && nthPrime(pIdx) <= sqrtR) {
                val p = nthPrime(pIdx)

                val smallestMultipleAfterL = ((l - 1 + p) / p) * p max 2 * p
                for (i <- smallestMultipleAfterL to r by p)
                  isPrime((i - l).toInt) = false

                loop(pIdx + 1)
              }
            }

            loop(0)
            subStream.filter(i => isPrime((i - l).toInt))
          }
        } #::: collectPrimes(remaining)
      }
    }

    collectPrimes(stream.dropWhile(_ < 2).takeWhile(_ >= 2))
  }

  def infiniteStream(from: Long, step: Int): Stream[Long] =
    from #:: infiniteStream(from + step, step)

  def main(args: Array[String]): Unit = {

    for (t <- 1 to io.StdIn.readInt) {
      val n     = io.StdIn.readLong()
      val sqrtN = math.sqrt(n).floor.toLong

      val Stream(above) = primalStream(infiniteStream(sqrtN + 1, 1)).take(1)
      val primesBelow   = primalStream(infiniteStream(sqrtN, -1)).take(2)

      val result = {
        primesBelow match {
          case below #:: Stream.Empty =>
            above * below
          case below1 #:: below2 #:: Stream.Empty if above * below1 <= n =>
            (above * below1) max (below1 * below2)
          case below1 #:: below2 #:: Stream.Empty =>
            below1 * below2
        }
      }

      printFormattedOutput(t, result)
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
