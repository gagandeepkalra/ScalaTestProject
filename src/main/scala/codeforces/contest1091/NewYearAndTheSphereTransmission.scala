package codeforces.contest1091

import scala.collection.SortedSet

/*

https://codeforces.com/contest/1091/problem/C

 */
object NewYearAndTheSphereTransmission {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val sqrt: Int = math.sqrt(n).floor.toInt

    println((1 to sqrt).foldLeft(SortedSet[Long]())((set, i) => {
      if (n % i == 0) {
        set + calculateSumOfSeries(1, n + 1 - i, i) + calculateSumOfSeries(1, n + 1 - n / i, n / i)
      } else set
    }).mkString(" "))

  }

  def calculateSumOfSeries(a: Int, an: Int, d: Int): Long = {
    val n = (an - a) / d + 1
    1l * n * (a + an) / 2
  }
}
