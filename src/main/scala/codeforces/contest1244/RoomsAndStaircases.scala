package codeforces.contest1244

/*
https://codeforces.com/contest/1244/problem/B
 */
object RoomsAndStaircases {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {

      val total = io.StdIn.readInt()
      val stairs = io.StdIn.readLine()

      val leftmost = stairs.indexOf('1')
      val rightmost = stairs.lastIndexOf('1')

      println {
        if (leftmost != -1 && rightmost != -1)
          ((rightmost + 1) * 2) max (total - rightmost) * 2 max total max ((leftmost + 1) * 2) max (total - leftmost) * 2
        else total
      }
    }
  }

}
