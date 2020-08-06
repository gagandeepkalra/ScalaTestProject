package codeforces.contests._1399

/*
https://codeforces.com/contest/1399/problem/D

state transition, we just have to track unique ids for open 0 ending and 1 ending lists
 */
object BinaryStringToSubsequences {
  def main(args: Array[String]): Unit = {
    for (_ <- 1 to io.StdIn.readInt) {
      val _ = io.StdIn.readInt()
      val input = io.StdIn.readLine.toList

      var id = 0

      def getUniqueId: Int = {
        id += 1
        id
      }

      @scala.annotation.tailrec
      def loop(ls: List[Char], zeroIds: List[Int] = Nil, oneIds: List[Int] = Nil, result: List[Int] = Nil): List[Int] = {
        ls match {
          case Nil => result.reverse
          case '0' :: tail => oneIds match {
            case Nil =>
              val id = getUniqueId
              loop(tail, id :: zeroIds, Nil, id :: result)
            case id :: oneTail =>
              loop(tail, id :: zeroIds, oneTail, id :: result)
          }
          case '1' :: tail => zeroIds match {
            case Nil =>
              val id = getUniqueId
              loop(tail, Nil, id :: oneIds, id :: result)
            case id :: zeroTail =>
              loop(tail, zeroTail, id :: oneIds, id :: result)
          }
        }
      }

      println {
        val ans = loop(input)
        s"$id\n${ans.mkString(" ")}"
      }
    }
  }
}
