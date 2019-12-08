/*
compress the given string, reducing k consecutive characters to nil
 */
object CompressString {

  def main(args: Array[String]): Unit = {
    println(compressWord("daabbbcccaaacccbbbaddeefffe", 3))

  }

  def compressWord(word: String, k: Int): String = {

    def encode(word: String): List[(Char, Int)] =
      word
        .foldLeft(List.empty[(Char, Int)]) { (acc, e) =>
          acc match {
            case Nil                        => (e, 1) :: Nil
            case (`e`, lastCharCount) :: xs => (e, lastCharCount + 1) :: xs
            case xs                         => (e, 1) :: xs
          }
        }
        .reverse

    def reduce(ls: List[(Char, Int)]): List[(Char, Int)] = {
      println(ls)
      ls match {
        case Nil                      => Nil
        case (_, i) :: tail if i == k => reduce(tail)

        case (c1, i1) :: (c2, i2) :: tail =>
          reduce((c2, i2) :: tail) match {
            case (c3, i3) :: tl if c1 == c3 =>
              if (i1 + i3 == k) tl else (c1, i1 + i3) :: tl
            case l =>
              (c1, i1) :: l
          }

        case _ =>
          ls
      }
    }

    reduce(encode(word)).map(x => x._1.toString * x._2).mkString("")
  }

}
