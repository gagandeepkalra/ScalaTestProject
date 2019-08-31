package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/B
 */
object BadPrices {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      println {
        val indexedSeq = io.StdIn.readLine.split(" ").map(_.toInt)

        (0 to n - 2)
          .foldRight((0, collection.mutable.TreeSet(indexedSeq.last))) {
            case (i, (res, set)) =>
              val v = indexedSeq(i)
              val minFound = set.until(v).nonEmpty

              (if (minFound) res + 1 else res, set += v)
          }
          ._1
      }
    }
  }

}
