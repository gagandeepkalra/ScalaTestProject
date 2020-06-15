package codeforces.contests._1367

/*
https://codeforces.com/contest/1367/problem/A
 */
object ShortSubstrings {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {

      val s = io.StdIn.readLine
      println(s.indices.by(2).map(s).mkString + s.last)
    }
  }

}
