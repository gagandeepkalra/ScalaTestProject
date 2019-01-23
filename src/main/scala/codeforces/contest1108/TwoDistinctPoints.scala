package codeforces.contest1108

/*
http://codeforces.com/contest/1108/problem/A
 */
object TwoDistinctPoints {
  def main(args: Array[String]): Unit = {
    val q = io.StdIn.readInt()
    (1 to q).foreach { _ =>
      val Array(l1, r1, l2, r2) = io.StdIn.readLine.split(" ").map(_.toInt)

      val result: List[(Int, Int)] = for {
        a <- List(l1, r1)
        b <- List(l2, r2)
        if a != b
      } yield (a, b)

      println(result.head._1 + " " + result.head._2)
    }
  }

}
