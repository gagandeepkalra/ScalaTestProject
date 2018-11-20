package GoogleCodeJamRoundD

object Q2 {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toLong)

      val Array(p1, p2, a1, b1, c1, m1) = io.StdIn.readLine.split(" ").map(_.toLong)
      val Array(h1, h2, a2, b2, c2, m2) = io.StdIn.readLine.split(" ").map(_.toLong)
      val Array(x1, x2, a3, b3, c3, m3) = io.StdIn.readLine.split(" ").map(_.toLong)
      val Array(y1, y2, a4, b4, c4, m4) = io.StdIn.readLine.split(" ").map(_.toLong)

      var arrPH, arrXY = new Array[(Long, Long)](n.toInt + 1)

      arrPH(1) = (p1, h1)
      arrPH(2) = (p2, h2)

      arrXY(1) = (x1, y1)
      arrXY(2) = (x2, y2)

      for (i <- 3 to n.toInt) {
        arrPH(i) = ((a1 * arrPH(i - 1)._1 + b1 * arrPH(i - 2)._1 + c1) % (m1 + 1), (a2 * arrPH(i - 1)._2 + b2 * arrPH(i - 2)._2 + c2) % (m2 + 1))
      }

      for (i <- 3 to k.toInt) {
        arrXY(i) = ((a3 * arrXY(i - 1)._1 + b3 * arrXY(i - 2)._1 + c3) % (m3 + 1), (a4 * arrXY(i - 1)._2 + b4 * arrXY(i - 2)._2 + c4) % (m4 + 1))
      }

      def isReachable(baloon: (Long, Long)): Boolean = {
        arrPH.exists { case (p, h) => (p <= baloon._1 && baloon._1 <= p + h || p - h <= baloon._1 && baloon._1 <= p) && (-baloon._1 + h >= baloon._2) }
      }

      System.out.println(s"Case #$t: " + arrXY.count(isReachable))
    }

  }

}
