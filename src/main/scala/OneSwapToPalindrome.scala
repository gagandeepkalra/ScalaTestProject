object OneSwapToPalindrome {

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(_ => {
      val s = io.StdIn.readLine

      var ls: List[Int] = Nil

      var i = 0
      var j = s.length - 1

      while (i < j) {
        if (s.charAt(i) != s.charAt(j)) ls = i :: ls
        i += 1
        j -= 1
      }

      ls match {
        case Nil => println("Yes")
        case x :: Nil => if (s.length % 2 == 1 && (s.charAt(x) == s.charAt(s.length / 2) || s.charAt(s.length - x - 1) == s.charAt(s.length / 2)))
          println("Yes") else println("No")
        case x :: y :: Nil => if (s.charAt(x) == s.charAt(s.length - y - 1) && s.charAt(y) == s.charAt(s.length - x - 1))
          println("Yes") else println("No")
        case _ => println("Yes")
      }

    })
  }
}
