
/*
Determine the winner
Assume there are two programmers with their hacker names as "Flash" and "Cisco". They both took part in a contest. The rules of the contest are:

There will be 4 questions to solve, P, Q, R and S.
Initial score (before the start of the contest) of the 4 problems is s_p, s_q, s_r and s_s.
After each minutes, the score of the questions, P, Q, R and S, will decrease by  d_p, d_q, d_r and d_s respectively. The score cannot decrease below half (integer division) of the initial score for each question i.e. at a particular time, the score of the problems will be maximum of half of the initial score and the decreased score.
Flash submitted the solutions of the questions at time f_p, f_q, f_r and f_s. Cisco submitted the solutions of the questions at time c_p, c_q, c_r and c_s. Your task is to find winner of contest. The winner is the one who has more score. If the score of both the programmers is same, then winner will be the one who took less time to solve all the questions. If both the problems have same score and took same time to solve the questions, then print Tie.

NOTE: All the times mentioned above are in minutes. Time taken to solve all the questions is the time at which programmers submitted the last solution.


Input Format

First line of input contains an integer T, denoting number of test cases.

First line of each test case contains 4 space separated integers s_p, s_q, s_r, s_s denoting the initial scores for each problems.

Second line of each test case contains 4 space separated integers d_p, d_q, d_r, d_s denoting the decrease in each problem's score after each minute.

Third line of each test case contains 4 space separated integers f_p, f_q, f_r, f_s denoting the time when Flash submitted his solutions.

Fourth line of each test case contains 4 space separated integers c_p, c_q, c_r, c_s denoting the time when Cisco submitted his solutions.



Output Format

Print the winner of the competition - Flash or Cisco. If both the problems have same score and took same time to solve the questions, then print Tie.



Constraints

1 <= T <= 105

100 <= s_p, s_q, s_r, s_s <= 106

0 <= d_p, d_q, d_r, d_s <= 106

0 <= f_p, f_q, f_r, f_s <= 106

0 <= c_p, c_q, c_r, c_s <= 106

Sample Input
1
1000 2000 3000 4000
1 2 30 40
110 10 7 8
15 30 45 20
Sample Output
Flash
Explanation
Flash submitted the solutions at time 110, 10, 7 and 8 for the questions P, Q, R and S, respectively. For question P he will get score of only 1000 - 110 * 1 = 890. For question Q he will get 2000 - 10 * 2 = 1980. Similarly, for question R and S, he will get 2790 and 3680. Therefore, the total score is 9340. Total time he took to solve all problems is 110.

Cisco submitted the solutions at time 15, 30, 45 and 55 for the questions P, Q, R and S, respectively. So he will get 985, 1940, 1650 and 3200 score for the questions P, Q, R and S, respectively. Total score is 7775. Total time he took to solve all problems is 45.

Since Flash's total score is greater than Cisco's, Flash is the winner.

 */

object DetermineTheWinner {

  def main(args: Array[String]): Unit = {

    def inputSequence: Array[Long] = io.StdIn.readLine.split(" ").map(_.toLong)

    (1 to io.StdIn.readInt()).foreach(_ => {

      val initialScores = inputSequence
      val decreasePerMinute = inputSequence

      val flashTime = inputSequence
      val flashScore: Long = initialScores.zip(decreasePerMinute).zip(flashTime)
        .map(x => math.max(x._1._1 / 2, x._1._1 - x._1._2 * x._2)).sum

      val ciscoTime = inputSequence
      val ciscoScore: Long = initialScores.zip(decreasePerMinute).zip(ciscoTime)
        .map(x => math.max(x._1._1 / 2, x._1._1 - x._1._2 * x._2)).sum

      if (flashScore == ciscoScore) {

        if (flashTime.max == ciscoTime.max) println("Tie")
        else println(if (flashTime.max < ciscoTime.max) "Flash" else "Cisco")

      } else println(if (flashScore > ciscoScore) "Flash" else "Cisco")

    })
  }

}
