package codeforces.contests._1359

/*
https://codeforces.com/contest/1359/problem/E

[Binary Exponentiation, Permutations, Combinations]

Order doesn't matter only if a1, a2, a3 are multiples of some a
 */
object ModularStability {

  val MOD = 998244353L

  def power(x: Long, y: Long): Long = {
    if (x == 0) 0
    else {
      var ans: Long = 1
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

  def main(args: Array[String]): Unit = {
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

    val MAX = n + 1

    val factorialOf: Array[Long] = {
      val f = new Array[Long](MAX)
      f(0) = 1
      (1 until MAX).foreach(i => f(i) = (i * f(i - 1)) % MOD)
      f
    }

    def inverseMod(a: Long): Long = power(a, MOD - 2)

    val inverseModOfFactorial: Array[Long] = new Array[Long](MAX)
    inverseModOfFactorial(MAX - 1) = inverseMod(factorialOf(MAX - 1))
    for (i <- MAX - 1 to 1 by -1) inverseModOfFactorial(i - 1) = (i * inverseModOfFactorial(i)) % MOD

    def nCr(n: Int, r: Int): Long = {
      if (r < 0 || n < r) 0
      else ((factorialOf(n) * inverseModOfFactorial(r) % MOD) * inverseModOfFactorial(n - r)) % MOD
    }

    println {
      (1 to n).foldLeft(0L) { (acc, i) =>
        if (i * k <= n) (acc + nCr(n / i - 1, k - 1)) % MOD else acc
      }
    }
  }

}
