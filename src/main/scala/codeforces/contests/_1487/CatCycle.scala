package codeforces.contests._1487

object CatCycle {

  def main(args: Array[String]): Unit = {
    println({
      for (_ <- 1 to io.StdIn.readInt()) yield {
        val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toLong)

        if ((n % 2) == 0) (k - 1) % n
        else {
          // 0 based
          val window = (k - 1) / (n - 1)
          val offset = (k - 1) % (n - 1)
          val start  = window % n

          if (offset < n / 2) (start + offset) % n else (start + offset + 1) % n
        }
      } + 1
    }.mkString("\n"))
  }
}
