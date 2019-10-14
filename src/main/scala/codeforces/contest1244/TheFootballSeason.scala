package codeforces.contest1244

/*
https://codeforces.com/contest/1244/problem/C
 */
object TheFootballSeason {

  def main(args: Array[String]): Unit = {
    val Array(n, p, w, d) = io.StdIn.readLine.split(" ").map(_.toLong)

    val nwp = n * w - p

    val r1 = nwp % w

    @scala.annotation.tailrec
    def findY(t: Int, r2s: Set[Long] = Set.empty): Option[Long] = {
      val numerator = r1 + t * w
      if (nwp - numerator >= 0) {
        val r2 = numerator % (w - d)
        if (r2 == 0)
          Some(numerator / (w - d))
        else if (r2s contains r2) None
        else findY(t + 1, r2s + r2)
      } else None
    }

    val maybeY = if (nwp >= 0) findY(0) else None

    println {
      {
        for {
          y <- maybeY
          z = (nwp - (w - d) * y) / w
          x = n - y - z
          if x >= 0
        } yield s"$x $y $z"
      }.getOrElse("-1")
    }
  }

}
