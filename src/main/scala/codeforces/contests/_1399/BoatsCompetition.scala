package codeforces.contests._1399

/*
https://codeforces.com/contest/1399/problem/C

We find for each possible total weight s, the #pairs which match then find max off of it
 */
object BoatsCompetition {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val a = io.StdIn.readLine.split(" ").map(_.toInt).sorted

      if (n >= 2) {
        val wF = new Array[Int](n + 1)
        a.foreach(wF(_) += 1)

        val ans = (2 to 2 * n).map { s => // foreach weight

          (1 to ((s - 1) min n)).foldLeft(0)((acc, w1) => acc + { // foreach w1, we add up the matching pairs
            val w2 = s - w1
            if (w2 <= n && w1 <= w2) (if (w1 < w2) wF(w1) min wF(w2) else wF(w1) / 2) else 0
          })

        }

        println(ans.max)
      } else println(0)
    }
  }
}
