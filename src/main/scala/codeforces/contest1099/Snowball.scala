package codeforces.contest1099

object Snowball {
  def main(args: Array[String]): Unit = {

    val Array(weight, height), Array(u1, d1), Array(u2, d2) = io.StdIn.readLine().split(" ").map(_.toInt)

    def recur(w: Int, h: Int): Int = {
      if (h == 0) w
      else {
        if (h == d1)
          recur((h + w - u1) max 0, h - 1)
        else if (h == d2)
          recur((h + w - u2) max 0, h - 1)
        else
          recur(h + w, h - 1)
      }
    }

    println(recur(weight, height))
  }

}
