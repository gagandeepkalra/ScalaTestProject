package codeforces.contests._1407

/*
https://codeforces.com/contest/1407/problem/B

Start with maximum element, iterate all for selecting every other element in the permutation, take by max gcd with current
 */
object BigVova {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val n = io.StdIn.readInt()
      val seq = io.StdIn.readLine.split(" ").map(_.toInt)

      @scala.annotation.tailrec
      def gcd(a: Int, b: Int): Int = {
        if (b == 0) a else gcd(b, a % b)
      }

      val maximumIdx = (0 until n).maxBy(seq)

      val done: collection.mutable.Set[Int] = collection.mutable.Set(maximumIdx)

      @scala.annotation.tailrec
      def loop(accGcd: Int = seq(maximumIdx), accRes: List[Int] = seq(maximumIdx) :: Nil): List[Int] = {
        if (done.size == n) accRes.reverse
        else {
          val toRemoveIdx = (0 until n).filter(!done(_)).maxBy(i => gcd(seq(i), accGcd))
          done += toRemoveIdx
          loop(gcd(seq(toRemoveIdx), accGcd), seq(toRemoveIdx) :: accRes)
        }
      }

      println(loop().mkString(" "))
    }
  }
}
