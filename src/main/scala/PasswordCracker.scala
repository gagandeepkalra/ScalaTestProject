object PasswordCracker {

  def main(args: Array[String]): Unit = {

    def checkIfConcatenatedString(loginAttempt: String, index: Int, passwords: Array[String], memo: Array[Int]): Boolean = {
      if (index == loginAttempt.length) {
        return true
      }

      if (memo(index) != -2) { // somebody did set the result
        return memo(index) != -1
      }


      val result = passwords.zipWithIndex
        .filter(index + _._1.length <= loginAttempt.length)
        .exists { case (password, i) =>
          if (loginAttempt.startsWith(password, index)) {
            if (checkIfConcatenatedString(loginAttempt, index + password.length, passwords, memo)) {
              memo(index) = i
              true
            } else false
          }
          else false
        }

      if (!result) memo(index) = -1

      result
    }

    (1 to io.StdIn.readInt()).foreach(_ => {
      val n = io.StdIn.readInt()
      val passwords = io.StdIn.readLine.split(" ")
      val loginAttempt = io.StdIn.readLine
      val memo = Array.fill[Int](loginAttempt.length)(-2)

      // -2 unset
      // -1 false
      // >=0 true

      if (checkIfConcatenatedString(loginAttempt, 0, passwords, memo)) {
        var i = 0
        while (i < loginAttempt.length) {
          print(passwords(memo(i)) + " ")
          i += passwords(memo(i)).length
        }
        println
      } else println("WRONG PASSWORD")
    })
  }
}
