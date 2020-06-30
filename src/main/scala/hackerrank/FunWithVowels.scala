package hackerrank

/*
Length of the longest subsequence made of vowels (each is required at-least once)
 */
object FunWithVowels {

  def longestVowelSubsequence(s: String): Int = {
    val n = s.length

    val dp = Array.ofDim[Int](5, n)
    dp(0)(0) = if (s(0) == 'a') 1 else 0

    def copyPrevColumn(c: Int): Unit = {
      (0 to 4).foreach(r => dp(r)(c) = dp(r)(c - 1))
    }

    (1 until n).foreach { i =>
      copyPrevColumn(i)
      s(i) match {
        case 'a' => dp(0)(i) += 1
        case 'e' if dp(0)(i) != 0 => dp(1)(i) = (dp(0)(i - 1) max dp(1)(i - 1)) + 1
        case 'i' if dp(1)(i) != 0 => dp(2)(i) = (dp(1)(i - 1) max dp(2)(i - 1)) + 1
        case 'o' if dp(2)(i) != 0 => dp(3)(i) = (dp(2)(i - 1) max dp(3)(i - 1)) + 1
        case 'u' if dp(3)(i) != 0 => dp(4)(i) = (dp(3)(i - 1) max dp(4)(i - 1)) + 1
        case _ =>
      }
    }

    dp(4)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    println(longestVowelSubsequence("aeiaaiooaa"))
    println(longestVowelSubsequence("aeiaaioooaauuaeiu"))
  }
}
