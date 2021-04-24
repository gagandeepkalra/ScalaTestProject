package google.kickstart._2019.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edb/00000000001707b9

count numbers in range [l, r] where diff of even and odd divisors is <= 2

[Sieve of Eratosthenes]

given n = x^p * y^q * z^r then // (x, y, z) are primes
#divisors = (p+1) * (q+1) * (r+1)
#odd_divisors = #divisors / ((a+1) in 2^a divides n)

Proposition: There's only one prime above √𝑛
Proof: Suppose 𝑛 is a positive integer s.t. 𝑛=𝑝𝑞, where 𝑝 and 𝑞 are prime numbers. Assume to the contrary 𝑝>√𝑛 and 𝑞>√𝑛.
Multiplying these inequalities we have 𝑝.𝑞>√𝑛.√𝑛, which implies 𝑝𝑞>𝑛. This is a contradiction to our hypothesis 𝑛=𝑝𝑞. Hence we can conclude
that either 𝑝≤√𝑛 or 𝑞≤√𝑛.
 */
object StreetCheckers {
  val primes: List[Int] = {
    val max = 32000 // sqrt(10^9)
    val sieve = Array.fill[Boolean](max)(true)
    for {
      i <- 2 until max
      if sieve(i)
      j <- 2 * i until max by i
    } sieve(j) = false

    (2 until max).filter(sieve).toList
  }

  @scala.annotation.tailrec
  private def countTwos(n: Int, acc: Int = 0): Int = // number of times can be divided by 2
    if (n > 0 && (n & 1) == 0) countTwos(n >> 1, acc + 1) else acc

  def solve(l: Int, r: Int): Int = {
    val divisors = Array.fill[Int](r - l + 1)(1)
    val sieve: Array[Int] = (l to r).toArray

    val sqrtR = math.sqrt(r).ceil.toInt

    @scala.annotation.tailrec
    def loop(primes: List[Int]): Unit =
      if (primes.nonEmpty && primes.head <= sqrtR) {
        val p = primes.head
        for {
          i <- ((l - 1) / p * p + p) to r by p
        } {
          val idx = i - l
          var c = 1
          while (sieve(idx) % p == 0) {
            c += 1
            sieve(idx) = sieve(idx) / p
          }
          divisors(idx) *= c
        }
        loop(primes.tail)
      }

    loop(primes)

    divisors.indices.foldLeft(0) { (acc, i) =>
      val total = {
        if (sieve(i) == 1) divisors(i) else divisors(i) * 2 // if prime, else there's one prime above √𝑛, divisors double in that case
      }
      val c = countTwos(i + l)
      val odd = total / (c + 1)
      val even = total - odd

      if (math.abs(even - odd) <= 2) acc + 1 else acc
    }
  }

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(l, r) = io.StdIn.readLine.split(" ").map(_.toInt)

      printFormattedOutput(t, solve(l, r))
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
