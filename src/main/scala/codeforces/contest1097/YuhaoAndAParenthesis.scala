package codeforces.contest1097

object YuhaoAndAParenthesis {
  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt()

    def valueOf(s: String): Int = s.foldLeft(0)((acc, c) => acc + (if (c == '(') 1 else -1))

    val positive, negative = new Array[Int](1000000)

    (1 to n).foreach(_ => {
      val value: Int = valueOf(io.StdIn.readLine())
      if (value >= 0) positive(value) += 1
      else negative(value) += 1
    })

  }
}
