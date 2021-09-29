package GoogleCodeJam2019Qualifications

object PowerArrangers {
  def main(args: Array[String]): Unit = {

    val Array(testCases, _) = io.StdIn.readLine().split(" ").map(_.toInt)

    for (t <- 1 to testCases) {

      val firstMap = ask(1 to 591 by 5) // 119

      val (firstChar, firstSeq) = firstMap.find(_._2.size != 24).get

      val secondMap = ask(firstSeq.map(_ + 1)) // 23

      val (secondChar, secondSeq) = secondMap.find(_._2.size != 6).get

      val thirdMap = ask(secondSeq.map(_ + 1)) // 5

      val (thirdChar, thirdSeq) = thirdMap.find(_._2.size != 2).get

      val fourthMap = ask(thirdSeq.map(_ + 1)) // 1

      val (fourthChar, fourthSeq) = fourthMap.find(_._2.size != 2).get

      val fifthChar = (Set('A', 'B', 'C', 'D', 'E') -- Set(firstChar, secondChar, thirdChar, fourthChar)).head

      val result = List(firstChar, secondChar, thirdChar, fourthChar, fifthChar).mkString("")
      println(result)

      val judgeResponse = io.StdIn.readChar()

      if(judgeResponse == 'N') System.exit(1)
    }


    def ask(questions: Iterable[Int]): Map[Char, List[Int]] = {
      questions.foldLeft(Map[Char, List[Int]]())((map, question) => {
        println(question)
        val char = io.StdIn.readChar()
        if (char == 'N') System.exit(1)
        map + (char -> (question :: map.getOrElse(char, List[Int]())))
      })
    }
  }
}
