package codeforces.contest1091

/*

https://codeforces.com/contest/1091/problem/A

 */
object NewYearAndTheChristmasOrnament {

  def main(args: Array[String]): Unit = {
    val Array(y, b, r) = io.StdIn.readLine.split(" ").map(_.toInt)
    println(sumOfMaxPossibleIncrementalSequence(y, b, r))
  }

  def sumOfMaxPossibleIncrementalSequence(y: Int, b: Int, r: Int): Int = {
    if (!requiresFix(y, b) && !requiresFix(b, r)) y + b + r
    else {
      if (requiresFix(b, r)) {
        val f = fix(b, r)
        sumOfMaxPossibleIncrementalSequence(y, f._1, f._2)
      } else {
        val f = fix(y, b)
        sumOfMaxPossibleIncrementalSequence(f._1, f._2, r)
      }
    }
  }

  def requiresFix(x: Int, y: Int): Boolean = x + 1 != y

  def fix(x: Int, y: Int): (Int, Int) = {
    if (x < y) (x, x + 1)
    else (y - 1, y)
  }
}
