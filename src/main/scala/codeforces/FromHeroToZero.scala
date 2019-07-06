package codeforces

/*
https://codeforces.com/contest/1175/problem/A
 */
object FromHeroToZero {

  def countStepsToZero(k: Long)(n: Long, result: Long): Long = {
    if (n == 0) result
    else {
      n % k match {
        case 0 => countStepsToZero(k)(n / k, result + 1)
        case r => countStepsToZero(k)(n - r, result + r)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine().split(" ").map(_.toLong)
      println(countStepsToZero(k)(n, 0))
    }
  }

}
