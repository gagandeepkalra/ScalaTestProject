package hackerrank.WeekOfCode37

import scala.io.StdIn

object AverageRating {

  def main(args: Array[String]): Unit = {
    val (s, l) = (1 to StdIn.readInt)
      .map(_ => StdIn.readInt)
      .filter(_ >= 90)
      .foldLeft[(Double, Int)]((0.0, 0)) { (acc, i) => (acc._1 + i, acc._2 + 1) }
    val ans: Double = s / l
    printf("%.2f", ans)
  }

}
