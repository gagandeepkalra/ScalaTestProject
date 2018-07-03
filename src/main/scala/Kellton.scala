object Kellton {

  /**
    * Given a 2D character array, print YES if all the characters could be arranged such that the 2D array is a palindrome.
    */
  def canFormPalindrome(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(_ => { // for each test case

      val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt) // row and column

      val frequency = new Array[Int](26)
      (1 to n).foreach(_ => {
        io.StdIn.readLine.toCharArray.foreach(c => frequency(c - 'a') += 1)
      })

      System.out.println(if (n % 2 == 0 && m % 2 == 0) { // both even
        if (frequency.map(_ / 4).sum == (n * m) / 4) "YES" else "NO"
      } else if (n % 2 != 0 && m % 2 == 0) { // n odd, m even
        if (frequency.map(_ / 4).sum >= ((n - 1) * m) / 4 && frequency.forall(_ % 2 == 0)) "YES" else "NO"
      } else if (n % 2 == 0 && m % 2 != 0) { // n even, m odd
        if (frequency.map(_ / 4).sum >= (n * (m - 1)) / 4 && frequency.forall(_ % 2 == 0)) "YES" else "NO"
      } else { // both odd
        if (frequency.map(_ / 4).sum >= ((n - 1) * (m - 1)) / 4 && frequency.count(_ % 2 != 0) == 1) "YES" else "NO"
      })

    })
  }
}
