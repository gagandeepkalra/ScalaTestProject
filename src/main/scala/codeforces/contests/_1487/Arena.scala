package codeforces.contests._1487

/**
 * http://codeforces.com/contest/1487/problem/A
 */
object Arena {

  def main(args: Array[String]): Unit = {
    println({
      for (_ <- 1 to io.StdIn.readInt()) yield {
        val n   = io.StdIn.readInt
        val arr = io.StdIn.readLine.split(" ").map(_.toInt)

        val minE = arr.min
        val mineECount = arr.count(_ == minE)

        if(n == mineECount) 0 else n - mineECount
      }
    }.mkString("\n"))
  }
}
