package google.kickstart._2020.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff47/00000000003bef73

We try generate a valid sequence and then validate it.

if a == b == c, we distribute on both ends then ones in the middle otherwise we pick either left or right, fill easy and
then distribute ones on the other side
 */
object HighBuildings {
  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, a, b, c) = io.StdIn.readLine.split(" ").map(_.toInt)

      val result = new Array[Int](n)

      if (n - a >= b - c) {

        if (a == b && b == c) {
          result(n - 1) = n
          (0 until c - 1).foreach(result(_) = n)
          (c - 1 until n - 1).foreach(result(_) = 1)
        }
        else if (b != c) {
          (a - c - 1 to 0 by -1).zip(n - 1 to 1 by -1).foreach { case (i, v) => result(i) = v }
          (a - c until a).foreach(result(_) = n)

          (a until n - (b - c)).foreach(i => result(i) = 1)
          (n - (b - c) until n).zip(n - 1 to 1 by -1).foreach { case (i, v) => result(i) = v }
        } else { // b == c
          (n - c until n).foreach(result(_) = n)
          (a - c until n - c).foreach(result(_) = 1)
          (a - c - 1 to 0 by -1).zip(n - 1 to 1 by -1).foreach { case (i, v) => result(i) = v }
        }

        val calA = result.foldLeft((0, Int.MinValue)) { case ((acc, maximum), v) => if (maximum <= v) (acc + 1, v) else (acc, maximum) }._1

        val calB = result.foldRight((0, Int.MinValue)) { case (v, (acc, maximum)) => if (maximum <= v) (acc + 1, v) else (acc, maximum) }._1

        val allInRange = result.forall(i => 1 <= i && i <= n)

        if (allInRange && calA == a && calB == b)
          printFormattedOutput(t, result.mkString(" "))
        else
          printFormattedOutput(t, "IMPOSSIBLE")

      } else {
        printFormattedOutput(t, "IMPOSSIBLE")
      }

    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
