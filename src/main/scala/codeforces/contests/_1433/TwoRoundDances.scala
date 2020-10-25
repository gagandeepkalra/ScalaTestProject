package codeforces.contests._1433

/*
https://codeforces.com/contest/1433/problem/E

[Permutations, Combinations]

Divide by two to disambiguate
 */
object TwoRoundDances {

  @scala.annotation.tailrec
  def fact(n: Long, acc: Long = 1L): Long = if (n == 0) acc else fact(n - 1, n * acc)

  def nCr(n: Long, r: Long): Long = (fact(n) / fact(r)) / fact(n - r)

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readLong()

    println {
      nCr(n, n / 2) * fact(n / 2 - 1) * fact(n / 2 - 1) / 2
    }
  }
}
