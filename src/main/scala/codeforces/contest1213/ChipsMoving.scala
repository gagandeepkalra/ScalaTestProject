package codeforces.contest1213

/*
https://codeforces.com/contest/1213/problem/A
 */
object ChipsMoving {

  def main(args: Array[String]): Unit = {
    val _ = io.StdIn.readInt()
    val (evens, odds) =
      io.StdIn.readLine.split(" ").map(_.toInt).foldLeft((0, 0)) {
        case ((e, o), i) =>
          if ((i & 1) == 1) (e, o + 1) else (e + 1, o)
      }
    println(evens min odds)
  }

}
