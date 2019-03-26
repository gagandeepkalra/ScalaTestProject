package codeforces.contest1099

object Postcard {

  def count(s: String, c: Char): Int = s.count(_ == c)

  def main(args: Array[String]): Unit = {
    val mssg = io.StdIn.readLine
    val k = io.StdIn.readInt()

    val questionMarks = count(mssg, '?')
    val stars = count(mssg, '*')

    val leastPossibleLength = mssg.length - questionMarks * 2 - stars * 2

    if (leastPossibleLength > k) println("Impossible")
    else if (stars == 0) {
      val maxPossibleLength = mssg.length - questionMarks
      if (k > maxPossibleLength) println("Impossible")
      else {
        val toKeepQuestionMarks = k - leastPossibleLength

        println((0 until toKeepQuestionMarks).foldLeft(mssg)((s, _) => s.replaceFirst("\\?", "")).replaceAll("\\w\\?", ""))
      }
    }
    else {
      val indexOfLastStar = mssg.length - mssg.reverse.indexWhere(_ == '*') - 1
      val toInsert = k - leastPossibleLength

      val result = mssg.substring(0, indexOfLastStar) + mssg(indexOfLastStar - 1).toString * toInsert + mssg.substring(indexOfLastStar)

      println(result.replaceAll("\\w[\\?\\*]", ""))
    }

  }

}
