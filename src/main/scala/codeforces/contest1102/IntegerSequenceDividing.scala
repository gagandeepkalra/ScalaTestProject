package codeforces.contest1102

object IntegerSequenceDividing {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    println(n % 4 match {
      case 0 => 0
      case 1 => 1
      case 2 => 1
      case 3 => 0
    })
  }
}
