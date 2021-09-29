package codeforces.contests._1462

object UniqueNumber {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()

      val (acc, r) = (9 to 1 by -1).foldLeft((List.empty[Int], n)) {
        case ((acc, r), i) =>
          if (r == 0) (acc, r)
          else if (r > i) {
            (i :: acc, r - i)
          } else { // r <= i
            (r :: acc, 0)
          }
      }

      if (r == 0) println(acc.mkString("")) else println(-1)
    }
  }
}
