package GoogleCodeJam2018RoundE

/*

Problem
The milk tea in China is very delicious. There are many binary ("either-or") options for customizing a milk tea order,
such as "with ice"/"no ice", "with sugar"/"no sugar", "with bubbles"/"no bubbles", "with pudding"/"no pudding", and so on.
A customer's preferences for their milk tea can be represented as a binary string. For example, using the four properties above
(in the order they are given), the string 1100 means "with ice, with sugar, no bubbles, no pudding".

Today, Shakti is on duty to buy each of his N friends a milk tea, at a shop that offers P binary options. But after collecting
everyone's preferences, Shakti found that the order was getting too complicated, so Shakti has decided to buy the same type of
milk tea for everyone. Shakti knows that for every friend, for every preference that is not satisfied, they will complain once.
For example, if two of the friends have preferences for types 101 and 010, and Shakti chooses type 001, then the first friend
will complain once and the second friend will complain twice, for a total of three complaints.

Moreover, there are M different forbidden types of milk tea that the shop will not make, and Shakti cannot choose any of those
forbidden types.

What is the smallest number of complaints that Shakti can get?

Input
The first line of the input gives the number of test cases, T. T test cases follow. Each test case starts with a line containing 3
integers N, M, and P, as described above. Then, there are N more lines, each of which contains a binary string; these represent the
preferences of the N friends. Finally, there are M more lines, each of which contains a binary string; these represent the forbidden
types of milk tea that the shop will not make. Binary strings consist only of 0 and/or 1 characters.

Output
For each test case, output one line containing Case #x: y, where x is the test case number (starting from 1) and y is the minimum
number of complaints that Shakti can get, per the rules described above.

 */
object Q2 {

  abstract class Trie {
    def insert(ls: List[Char]): Trie = {
      ls match {
        case Nil => Node()
        case h :: tail =>
          h match {
            case '0' =>
              this match {
                case Leaf => Node(Array(insert(tail), Leaf))
                case Node(children) => Node(Array(children(0).insert(tail), children(1)))
              }
            case '1' =>
              this match {
                case Leaf => Node(Array(Leaf, insert(tail)))
                case Node(children) => Node(Array(children(0), children(1).insert(tail)))
              }
          }
      }
    }
  }

  case class Node(children: Array[Trie] = Array(Leaf, Leaf)) extends Trie

  object Leaf extends Trie

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, m, p) = io.StdIn.readLine().split(" ").map(_.toInt)

      val cost: Seq[(Int, Int)] = (1 to n).map(_ => io.StdIn.readLine().map(c => (c - '0', 1 - (c - '0'))))
        .reduceLeft((res, elm) => res.zip(elm).map(x => (x._1._1 + x._2._1, x._1._2 + x._2._2))) // cost of (including zero, including one) for each i <- 0 unitl p

      val costSumSuffix: Seq[Int] = cost.scanRight(0)((e, res) => res + math.min(e._1, e._2))

      val trie = (1 to m).map(_ => io.StdIn.readLine().toList).foldLeft[Trie](Node())((trie, ls) => trie.insert(ls))

      println(s"Case #$t: " + solve(trie, 0, 0))

      def solve(trie: Trie, i: Int, res: Int): Int = {
        trie match {
          case Leaf => res + costSumSuffix(i)
          case Node(children) => if (i == p) Int.MaxValue
          else math.min(solve(children(0), i + 1, res + cost(i)._1), solve(children(1), i + 1, res + cost(i)._2))
        }
      }
    }
  }
}
