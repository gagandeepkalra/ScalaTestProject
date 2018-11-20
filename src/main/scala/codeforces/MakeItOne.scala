package codeforces

object MakeItOne {

  def gcd(a: Int, b: Int): Int = {
    if (a == 0)
      b
    else
      gcd(b % a, a)
  }

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)

    var i = 0
    var j = 1

    var gcd = arr(0)
    var res = 0

    while (i < n) {

//      while (j < n && gcd != 1) {
//        gcd = gcd(gcd, arr(j))
//        j += 1
//      }
//
//      while (i < n && gcd == 1) {
//
//      }

    }
  }
}

