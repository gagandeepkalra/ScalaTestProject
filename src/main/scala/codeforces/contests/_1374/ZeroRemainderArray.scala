package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/D
 */
object ZeroRemainderArray {
  def main(args: Array[String]): Unit = {
    println {
      {
        for (_ <- 1 to io.StdIn.readInt()) yield {
          val Array(_, k) = io.StdIn.readLine.split(" ").map(_.toLong)

          val arr = io.StdIn.readLine.split(" ").map { ai =>
            val r = ai.toLong % k
            if (r == 0) 0 else k - r
          }.groupBy(identity) // minimum amount need to make divisible by k

          arr.foldLeft(0L) { case (acc, (i, is)) =>
            val c = is.length
            if (i == 0) acc else acc max (i + (c - 1) * k + 1)
          }
        }
      }.mkString("\n")
    }
  }
}
