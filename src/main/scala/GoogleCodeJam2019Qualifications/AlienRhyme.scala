package GoogleCodeJam2019Qualifications

object AlienRhyme {

  def main(args: Array[String]): Unit = {


    val testCases = io.StdIn.readInt()

    for (t <- 1 to testCases) {
      val n = io.StdIn.readInt()

      val arr = new Array[String](n)

      for (i <- 0 until n) arr(i) = io.StdIn.readLine().reverse

      println(s"Case #$t: ${count(arr.sorted.toList, "")}")


    }

  }


  def count(list: List[String], lastMatch: String): Int = {
    list match {
      case Nil => 0
      case s :: Nil => 0
      case a :: b :: ls =>
        maxCommonPrefix(a, b) match {
          case Some(s) => findUnusedPart(s, lastMatch).map(unusedPart => 2 + count(ls, unusedPart)).getOrElse(count(b :: ls, lastMatch))
          case None => count(b :: ls, lastMatch)
        }
    }
  }

  def maxCommonPrefix(a: String, b: String): Option[String] = {
    val count = a.zip(b).takeWhile { case (x, y) => x == y }.length

    if (count != 0) Some(a.take(count)) else None
  }

  def findUnusedPart(fromString: String, usedPart: String): Option[String] = {
    if (fromString == usedPart) {
      val res = fromString.dropRight(1)
      if (res.nonEmpty) Some(res) else None
    } else Some(fromString)
  }

}
