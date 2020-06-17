package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/D

We will construct the string 𝑡, starting with the largest letters. Note that if 𝑏𝑖=0, then the 𝑖-th letter of the string 𝑡
is maximal, so we know that the 𝑖-th letter affect all 𝑏𝑗≠0. While the string 𝑡 is not completely constructed, we will
do the following:

Find all 𝑖 such that 𝑏𝑖=0 and the 𝑖-th character of string 𝑡 is not placed;
Put on all these positions 𝑖 in the string 𝑡 the maximum letter not used in the string 𝑡 (there should be a sufficient
number of letters in the string 𝑠);
Subtract |𝑖−𝑗| from all 𝑏𝑗≠0.
 */
object TaskOnTheBoard {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val chars: List[(Char, Int)] = {
        val freq = new Array[Int](26)
        io.StdIn.readLine.foreach { c => freq(c - 'a') += 1 }

        freq.indices.map(i => ('a' + i).toChar -> freq(i)).reverse.toList
      }

      val m = io.StdIn.readInt

      val result = new Array[Char](m)

      val ms: Array[Int] = io.StdIn.readLine.split(" ").map(_.toInt)

      @scala.annotation.tailrec
      def loop(chars: List[(Char, Int)]): Unit = {
        val zeroIndexes = ms.indices.filter(ms(_) == 0)

        if (zeroIndexes.nonEmpty) {
          val (c, count) :: tail = chars

          if (count >= zeroIndexes.length) {

            zeroIndexes.foreach { i => ms(i) = -1; result(i) = c }

            zeroIndexes.foreach { z =>
              (z + 1 until m).foreach { r =>
                if (ms(r) > 0) {
                  ms(r) -= r - z
                }
              }

              (z - 1 to 0 by -1).foreach { l =>
                if (ms(l) > 0) {
                  ms(l) -= z - l
                }
              }
            }
          }
          loop(tail)
        }
      }

      loop(chars)

      println(result.mkString(""))
    }
  }

}
