package codeforces.contests._1407

/*
https://codeforces.com/contest/1407/problem/A

We get rid of all zeroes or ones
 */
object Ahahahahahahahaha {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val n = io.StdIn.readInt()
      val seq = io.StdIn.readLine.split(" ").map(_.toInt)

      val ones = seq.count(_ == 1)

      if (ones <= n / 2) {
        println(s"${n / 2}\n${"0" * (n / 2)}")
      } else {
        val req = if ((n / 2) % 2 == 0) n / 2 else n / 2 + 1
        println(s"$req\n${"1" * req}")
      }
    }
  }
}
