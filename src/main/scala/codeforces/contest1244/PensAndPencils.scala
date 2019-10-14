package codeforces.contest1244

/*
https://codeforces.com/contest/1244/problem/A
 */
object PensAndPencils {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val Array(lectures, practicals, lRate, pRate, limit) =
        io.StdIn.readLine.split(" ").map(_.toInt)

      val pensRequired = (lectures + (if (lectures % lRate == 0) 0 else lRate)) / lRate
      val pencilsRequired = (practicals + (if (practicals % pRate == 0) 0 else pRate)) / pRate

      if (pensRequired + pencilsRequired <= limit)
        println(s"$pensRequired $pencilsRequired")
      else
        println(-1)

    }
  }

}
