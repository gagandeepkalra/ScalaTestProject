package codeforces.contest1097

object PetrAndACombinationLock {

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()
    val a = new Array[Int](n)
    (0 until n).foreach(i => a(i) = io.StdIn.readInt())

    def recur(i: Int, deg: Int): Boolean = {
      if (i == n) deg == 0
      else {
        recur(i + 1, (deg + a(i)) % 360) || recur(i + 1, (deg + 360 - a(i)) % 360)
      }
    }

    if (recur(0, 0))
      println("YES")
    else
      println("NO")

  }
}
