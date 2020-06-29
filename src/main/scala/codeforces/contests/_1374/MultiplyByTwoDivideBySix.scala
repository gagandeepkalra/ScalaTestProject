package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/B
 */
object MultiplyByTwoDivideBySix {

  @scala.annotation.tailrec
  def countFactors(n: Int, m: Int, acc: Int = 0): Int =
    if (n == 1 || n % m != 0) acc else countFactors(n / m, m, acc + 1)

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val twos = countFactors(n, 2)
      val threes = countFactors(n, 3)

      val noOtherFactors = (math.pow(2, twos) * math.pow(3, threes)).toInt == n

      println {
        if (n == 1) 0
        else if (noOtherFactors && threes >= twos) threes + (threes - twos)
        else -1
      }
    }
  }
}
