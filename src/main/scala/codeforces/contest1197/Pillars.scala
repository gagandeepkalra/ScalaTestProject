package codeforces.contest1197

/*
https://codeforces.com/contest/1197/problem/B
 */
object Pillars {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val seqWithIndex: Array[(Int, Int)] = io.StdIn.readLine().split(" ").map(_.toInt).zipWithIndex

    val (maxValue, index) = seqWithIndex.maxBy(_._1)

    val seq = seqWithIndex.map(_._1)

    def loop(top: Int, l: Int, r: Int): Boolean = {
      val lProspect = if (l >= 0) Some(seq(l)) else None
      val rProspect = if (r < n) Some(seq(r)) else None

      (lProspect, rProspect) match {
        case (Some(ll), Some(rr)) if ll < top && rr < top && ll != rr =>
          if (ll > rr) loop(ll, l - 1, r) else loop(rr, l, r + 1)
        case (Some(ll), None) if ll < top =>
          loop(ll, l - 1, n)
        case (None, Some(rr)) if rr < top =>
          loop(rr, -1, r + 1)
        case (None, None) =>
          true
        case _ =>
          false
      }
    }

    println(if (loop(maxValue, index - 1, index + 1)) "YES" else "NO")
  }

}
