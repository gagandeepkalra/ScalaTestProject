object StringManipulation {

  def main(args: Array[String]): Unit = {
    val strA: String = io.StdIn.readLine

    println(removePreviouslyOccurringCharacters(strA))
  }

  def removePreviouslyOccurringCharacters(s: String): String = {
    s.distinct
  }

  def commonPrefixIndex(strA: String, strB: String, i: Int): Int = {
    if (i >= strA.size || i >= strB.size || strA(i) != strB(i)) i
    else commonPrefixIndex(strA, strB, i + 1)
  }


  def compress(s: String, i: Int, count: Int, prev: Char): Unit = {

    if (i >= s.length) {
      print(prev)
      if (count != 1) print(count)
    }
    else if (prev == s.charAt(i)) compress(s, i + 1, count + 1, prev)
    else {
      print(prev)
      if (count != 1) print(count)
      compress(s, i + 1, 1, s(i))

    }
  }

  def reverseAlternateElements() = {
    (0 until io.StdIn.readInt()).foreach(_ => {
      val strA: String = io.StdIn.readLine()

      println((0 until strA.size by 2).foldLeft(List[Char]())((acc, i) => {
        strA(i) :: strA(i + 1) :: acc
      }).reverse.mkString(""))

    })
  }
}
