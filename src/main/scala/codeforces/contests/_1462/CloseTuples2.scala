package codeforces.contests._1462

/**
 * https://codeforces.com/contest/1462/problem/E2
 *
 * You are given a sequence 𝑎 of length 𝑛 consisting of integers from 1 to 𝑛. The sequence may contain duplicates (i.e. some elements can be equal).
 *
 * Find the number of tuples of 𝑚 elements such that the maximum number in the tuple differs from the minimum by no more than 𝑘. Formally,
 * you need to find the number of tuples of 𝑚 indices 𝑖1<𝑖2<…<𝑖𝑚, such that
 *
 * max(𝑎𝑖1,𝑎𝑖2,…,𝑎𝑖𝑚)−min(𝑎𝑖1,𝑎𝑖2,…,𝑎𝑖𝑚)≤𝑘.
 */
object CloseTuples2 {

  import scala.annotation.tailrec

  val maxCount = 200001
  val MOD      = 1000000007

  def power(x: Long, y: Long): Long = {
    if (x == 0) 0
    else {
      var ans: Long  = 1
      var temp: Long = x

      var i = y
      while (i > 0) {
        if ((i & 1) == 1) {
          ans = (ans * temp) % MOD
        }
        temp = (temp * temp) % MOD
        i = i / 2
      }

      ans
    }
  }

  val factorialOf: Array[Long] = new Array[Long](maxCount)
  factorialOf(0) = 1
  for (i <- 1 until maxCount) factorialOf(i) = (i * factorialOf(i - 1)) % MOD

  val inverseModOfFactorial: Array[Long] = new Array[Long](maxCount)
  inverseModOfFactorial(maxCount - 1) = power(factorialOf(maxCount - 1), MOD - 2)

  for (i <- maxCount - 1 to 1 by -1) inverseModOfFactorial(i - 1) = (i * inverseModOfFactorial(i)) % MOD

  def nCr(n: Int, r: Int): Long = {
    if (r < 0 || n < r) 0
    else ((factorialOf(n) * inverseModOfFactorial(r) % MOD) * inverseModOfFactorial(n - r)) % MOD
  }

  def main(args: Array[String]): Unit = {
    for (_ <- 0 until io.StdIn.readInt) {
      val Array(n, m, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      val a = io.StdIn.readLine.split(" ").map(_.toInt).sorted

      // find rightmost index with value <= key
      @tailrec
      def binarySearch(key: Int, l: Int, r: Int): Int = {
        if (l > r) r
        else {
          val m = (l + r) / 2

          if (key < a(m)) binarySearch(key, l, m - 1)
          else binarySearch(key, m + 1, r)
        }
      }

      println(
        // find number of tuples given ith index is included
        a.indices
          .foldLeft(0L)((acc, i) => {
            val value  = a(i)
            val j      = binarySearch(value + k, i + 1, n - 1)
            val result = nCr(j - i, m - 1)
            (acc + result) % MOD
          })
      )

    }
  }
}
