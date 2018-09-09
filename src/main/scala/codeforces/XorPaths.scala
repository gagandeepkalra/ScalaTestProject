package codeforces

import scala.collection.mutable

/*

link- http://codeforces.com/problemset/problem/1006/F

There is a rectangular grid of size n×m. Each cell has a number written on it; the number on the cell (i,j) is a(i,j).
Your task is to calculate the number of paths from the upper-left cell (1,1) to the bottom-right cell (n,m) meeting the
following constraints:

You can move to the right or to the bottom only. Formally, from the cell (i,j) you may move to the
cell (i,j+1) or to the cell (i+1,j). The target cell can't be outside of the grid. The xor of all the numbers on the path
from the cell (1,1) to the cell (n,m) must be equal to k (xor operation is the bitwise exclusive OR, it is represented
as '^' in Java or C++ and "xor" in Pascal).Find the number of such paths in the given grid.InputThe first line of the
input contains three integers n, m and k (1≤n,m≤20, 0≤k≤1018) — the height and the width of the grid, and the number k.

The next n lines contain m integers each, the j-th element in the i-th line is ai,j (0≤ai,j≤1018). OutputPrint one integer-
the number of paths from (1,1) to (n,m) with xor sum equal to k.

Meet in the Middle

 */
object XorPaths {

  def main(args: Array[String]): Unit = {

    var arr = io.StdIn.readLine.split(" ").map(_.toLong)
    val n = arr(0).toInt
    val m = arr(1).toInt
    val k = arr(2)

    val source = Array.ofDim[Long](n, m)
    for (i <- 0 until n) io.StdIn.readLine.split(" ").map(_.toLong).zipWithIndex.foreach { case (x, j) => source(i)(j) = x }

    def recurForward(x: Int, y: Int, xor: Long, depth: Int)(implicit middle: mutable.Map[(Int, Int), mutable.Map[Long, Int]]): Unit = {
      if (depth == 1) {
        val res = xor ^ source(x)(y)
        val count: mutable.Map[Long, Int] = middle.getOrElseUpdate((x, y), collection.mutable.Map[Long, Int]())
        count(res) = count.getOrElse(res, 0) + 1

      } else {
        if (x + 1 < n) {
          recurForward(x + 1, y, xor ^ source(x)(y), depth - 1)
        }
        if (y + 1 < m) {
          recurForward(x, y + 1, xor ^ source(x)(y), depth - 1)
        }
      }
    }

    def recurBackward(x: Int, y: Int, xor: Long, depth: Int)(implicit middle: mutable.Map[(Int, Int), mutable.Map[Long, Int]]): Long = {
      if (depth == 1) middle.getOrElse((x, y), collection.mutable.Map[Long, Int]()).getOrElse(xor ^ k, 0).toLong
      else {
        var ans: Long = 0
        if (x - 1 >= 0) {
          ans += recurBackward(x - 1, y, xor ^ source(x)(y), depth - 1)
        }
        if (y - 1 >= 0) {
          ans += recurBackward(x, y - 1, xor ^ source(x)(y), depth - 1)
        }

        ans
      }
    }

    implicit val middle: mutable.Map[(Int, Int), mutable.Map[Long, Int]] = collection.mutable.Map[(Int, Int), collection.mutable.Map[Long, Int]]()
    val depth = (n + m) >> 1

    recurForward(0, 0, 0, depth)
    println(recurBackward(n - 1, m - 1, 0, n + m - depth))
  }
}
