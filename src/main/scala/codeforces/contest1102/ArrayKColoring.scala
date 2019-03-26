package codeforces.contest1102

object ArrayKColoring {
  def main(args: Array[String]): Unit = {
    val Array(n, k), seq = io.StdIn.readLine.split(" ").map(_.toInt)

    val groups: Map[Int, Array[Int]] = seq.zipWithIndex.groupBy(_._1).map { case (x, set) => (x, set.unzip._2) }

    if (groups.exists(_._2.length > k)) println("NO")
    else {

      var color = 0
      groups.foreach { case (v, indices) =>
        indices.foreach(i => {
          seq(i) = color
          color = (color + 1) % k
        })
      }

      println("YES")
      println(seq.map(_ + 1).mkString(" "))

    }
  }
}
