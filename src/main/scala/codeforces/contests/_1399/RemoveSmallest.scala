package codeforces.contests._1399

/*
https://codeforces.com/contest/1399/problem/A

YES if sorted sequence only increases by at-most 1 step every time
 */
object RemoveSmallest {

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val arr = io.StdIn.readLine.split(" ").map(_.toInt).sorted

      val doFollow = arr.foldLeft((true, arr.head)) { case ((acc, prev), e) => if (acc) (e - prev <= 1, e) else (false, e) }._1

      if (doFollow) println("YES") else println("NO")
    }
  }

}
