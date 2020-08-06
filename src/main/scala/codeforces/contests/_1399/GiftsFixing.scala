package codeforces.contests._1399

/*
https://codeforces.com/contest/1399/problem/B

reduce min from each and accumulate
 */
object GiftsFixing {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val a, b = io.StdIn.readLine.split(" ").map(_.toInt)

      val minA = a.min
      val minB = b.min

      val answer = (0 until n).foldLeft(0L)((acc, i) => acc + {

        val decA = a(i) - minA
        val decB = b(i) - minB

        decA max decB
      })

      println(answer)
    }
  }
}
