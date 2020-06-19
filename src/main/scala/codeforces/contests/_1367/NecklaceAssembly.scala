package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/E

we divide in bucket slots from 1 to n, save how much each bucket can be full
e.g bucket(3) = if we divide in three buckets how mich each will be full
 */
object NecklaceAssembly {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val s = io.StdIn.readLine

      val freq = new Array[Int](26)
      s.foreach(c => freq(c - 'a') += 1)

      val buckets = new Array[Int](n + 1)
      freq.foreach { f =>
        for (i <- 1 to f) buckets(i) += f / i
      }

      println {
        (1 to n).foldLeft(0) { (acc, i) =>
          acc max {
            val x = if (buckets(i) > 0) i * (buckets(i) to 1 by -1).find(k % _ == 0).get else 0
            val y = if (buckets(i) >= k) i * k else 0
            val z = if (k % i == 0) i else 0
            val a = if (buckets(i) >= 1) i else 0
            x max y max z max a
          }
        }
      }
    }
  }
}
