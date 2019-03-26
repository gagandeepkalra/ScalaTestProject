package codeforces.contest1102

object DoorsBreakingAndRepairing {
  def main(args: Array[String]): Unit = {
    val Array(n, x, y), seq = io.StdIn.readLine.split(" ").map(_.toInt)

    if (x > y) println(seq.length)
    else println((seq.count(_ <= x) + 1) / 2)

  }
}
