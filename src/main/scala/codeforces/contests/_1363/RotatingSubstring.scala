package codeforces.contests._1363

/*
https://codeforces.com/contest/1363/problem/F

[Dynamic Programming, Rotations]

if allowed both left and right rotations, in one step we can move a single char to it's final position, afterwards we
won't be moving it again, then there will be certain character sets already ordered (subsequence),
again we don't need to move them, just adjust everybody in between the gaps, ordered movements. That's why we look for
the longest common subsequence, the answer will (n - lcs).

e.g. "a e c b d" to "a b c d e", LCS = "a c d", answer = 2; "a e c b d" -right> "a e b c d" -left> "a b c d e"

Now the given case when only right rotations are permitted, everything's same only that we can add one more restriction.

Right rotation means, we can move any character to it's left.

Consider a stationary character s i and its destination counterpart in the second string t j. Respective to s i, we are
only allowed to move characters from right part of s to the left.

Consider a symbol x. Count its occurrences to the right of s i and t j, denote the counts A and B. The above reasoning
must imply A ≥ B. For s i and t j to be possibly matched, this condition must hold for all symbols.

The idea is that the subsequence we choose given this constraint would already accommodate the condition that we have
access to right only rotations.

dp(i)(j), minimum steps to convert s[1..i] to t[1..j], the only case we add a cost is when we pick an element from s, dp(i - 1)(j) + 1.
The remaining characters forms the desired subsequence

 */
object RotatingSubstring {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt()) {
      val n = io.StdIn.readInt()
      val s, t = '0' + io.StdIn.readLine()

      val (freqS, freqT) = {
        val fS, fT = new Array[Int](26)
        (1 to n).foreach { k =>
          fS(s(k) - 'a') += 1
          fT(t(k) - 'a') += 1
        }
        (fS, fT)
      }

      val frequenciesMatch = (0 until 26).forall(i => freqS(i) == freqT(i))

      println {
        if (frequenciesMatch) {
          val dp = Array.ofDim[Int](n + 1, n + 1)
          val fS = freqS.clone()
          (1 to n).foreach { i =>
            val fT = freqT.clone()
            fS(s(i) - 'a') -= 1

            (1 to n).foreach { j =>
              fT(t(j) - 'a') -= 1

              dp(i)(j) = if (s(i) == t(j)) dp(i - 1)(j - 1) else {
                (dp(i - 1)(j) + 1) min {
                  val x = t(j) - 'a'
                  if (fS(x) > fT(x)) dp(i)(j - 1) else Int.MaxValue
                }
              }
            }
          }

          dp(n)(n)
        } else -1
      }
    }
  }
}
