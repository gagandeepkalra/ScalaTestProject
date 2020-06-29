package codeforces.contests._1374

/*
https://codeforces.com/contest/1374/problem/C
 */
object MoveBrackets {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val _ = io.StdIn.readInt()
      val s = io.StdIn.readLine().toList


      @scala.annotation.tailrec
      def loop(ls: List[Char], score: Int = 0, acc: Int = 0): Int = {
        ls match {
          case Nil => acc
          case '(' :: tail => loop(tail, score + 1, acc)
          case ')' :: tail if score == 0 => loop(tail, score, acc + 1)
          case ')' :: tail => loop(tail, score - 1, acc)
        }
      }

      println(loop(s))
    }
  }
}
