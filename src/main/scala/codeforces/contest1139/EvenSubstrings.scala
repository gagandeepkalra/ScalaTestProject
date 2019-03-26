package codeforces.contest1139

object EvenSubstrings {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val string = io.StdIn.readLine()

    println(string.map(_ - '0').zipWithIndex.filter(_._1 % 2 == 0).map(_._2 + 1).sum)
  }

}
