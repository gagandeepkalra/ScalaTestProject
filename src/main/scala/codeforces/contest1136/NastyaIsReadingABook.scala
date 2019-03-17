package codeforces.contest1136

/**
  * http://codeforces.com/contest/1136/problem/A
  */
object NastyaIsReadingABook {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    val pages = readLines(n)

    val lastUnreadPage = io.StdIn.readInt()

    val chapterToRead = pages.indexWhere { case (a, b) => a <= lastUnreadPage && lastUnreadPage <= b } // 0 based

    println(n - chapterToRead)
  }

  def readLines(n: Int): List[(Int, Int)] = {
    def readInput(i: Int, res: List[(Int, Int)]): List[(Int, Int)] = {
      if (i >= n)
        res.reverse
      else {
        val Array(a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
        readInput(i + 1, (a, b) :: res)
      }
    }
    readInput(0, List())
  }

}
