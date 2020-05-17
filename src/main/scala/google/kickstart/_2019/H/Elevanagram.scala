package google.kickstart._2019.H

/*
https://codingcompetitions.withgoogle.com/kickstart/round/0000000000050edd/00000000001a286d

[Dynamic Programming]

for ai values >= 20, we reduce them to 20 or 21
each ai has to be divided into positives and negatives, and we have to consider resulting sum modulus 11

Now, if a[i] >= 20, then for each j in [0, 10] there will exist a value in sums such that value%11 == j. And,
[0, 10] is nothing but range of (any number)%11.

Let's take an example where i = 1, and a[i] = 20.

Sums will be: [0, 2, 4, 8, 10, 12, 14, 16, 18, 20]

Sums%11: [0, 2, 4, 8, 10, 1, 3, 5, 7, 9], which basically covers all numbers in range [0, 10].

And, since we only care about sums modulo 11, we therefore have covered all the possible sums. Hence we can
reduce a[i] to 20 or 21 on the basis of their parity

Afterwards we use digit dp to consider each sequence of choice
 */
object Elevanagram {

  def main(args: Array[String]): Unit = {
    println {
      (for (t <- 1 to io.StdIn.readInt()) yield {
        val arr = 0 +: io.StdIn
          .readLine()
          .split(" ")
          .map(_.toInt)
          .map(x => x min (if (x % 2 == 0) 20 else 21))

        val Sum = 21 * 10 // offset to cover for negative sum

        val Idx = arr.sum

        val dp = Array.fill[Option[Boolean]](Idx, Sum * 2)(None) // index, sum

        def loop(idx: Int, sum: Int): Boolean = {

          if (idx == Idx)
            sum % 11 == 0
          else
            dp(idx)(sum + Sum).getOrElse {
              val b: Boolean = (1 to 9).toStream
                .filter(arr(_) >= 1)
                .foldLeft(false)(
                  (acc, i) =>
                    acc || {
                      arr(i) -= 1
                      val b = loop(idx + 1, sum + i * (if (idx % 2 == 0) 1 else -1))
                      arr(i) += 1
                      b
                    }
                )

              dp(idx)(sum + Sum) = Some(b)
              b
            }
        }

        s"Case #$t: ${if (loop(0, 0)) "YES" else "NO"}"
      }).mkString("\n")
    }
  }

}
