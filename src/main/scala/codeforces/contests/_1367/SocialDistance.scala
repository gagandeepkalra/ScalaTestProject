package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/C

[Sliding Window]
 */
object SocialDistance {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
      val arr = io.StdIn.readLine.zipWithIndex

      val occupied = arr.collect { case (c, i) if c == '1' => i + 1 }.toVector

      println {
        if (occupied.isEmpty)
          (1 to n by k + 1).size
        else {
          val first = occupied.head
          val last = occupied.last

          val a = (1 until first by k + 1).count(_ + k < first)
          val b = occupied.sliding(2).map { case Vector(x, y) => (x + k + 1 until y).by(k + 1).count(_ + k < y); case _ => 0 }.sum
          val c = (last + k + 1 to n by k + 1).size
          a + b + c
        }
      }
    }
  }

}
