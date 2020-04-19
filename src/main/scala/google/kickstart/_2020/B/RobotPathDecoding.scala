package google.kickstart._2020.B

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc8/00000000002d83dc

[Recursion]

lookahead parser, context sensitive grammar

reduce expression and compute offset e.g 2(3(NW)2(W2(EE)W))
 */
object RobotPathDecoding {
  val min = 0
  val max = 1000000000

  val isDirection: Set[Char] = Set('N', 'S', 'E', 'W')
  val isInt: Set[Char] = (1 to 9).map(_ + '0').map(_.toChar).toSet

  val isOpeningBrace: Char => Boolean = _ == '('
  val isClosingBrace: Char => Boolean = _ == ')'

  def add(acc: (Int, Int), c: Char): (Int, Int) = {
    val (row, col) = acc
    c match {
      case 'N' => (if (row == min) max - 1 else row - 1, col)
      case 'S' => ((row + 1) % max, col)
      case 'E' => (row, (col + 1) % max)
      case 'W' => (row, if (col == min) max - 1 else col - 1)
    }
  }

  def add(acc: (Int, Int), group: (Int, Int)): (Int, Int) = ((acc._1 + group._1) % max, (acc._2 + group._2) % max)

  @scala.annotation.tailrec
  def addN(a: Int, times: Int, result: Int = 0): Int = {
    if (times == 0) result
    else addN(a, times - 1, (result + a) % max)
  }

  def multiply(acc: (Int, Int), i: Int): (Int, Int) = (addN(acc._1, i), addN(acc._2, i))

  def parse(p: List[Char], acc: (Int, Int) = (0, 0)): ((Int, Int), List[Char]) = {
    p match {
      case Nil => (acc, Nil)
      case c :: tail =>
        if (isDirection(c))
          parse(tail, add(acc, c))
        else if (isInt(c)) {
          val factor: Int = c - '0'
          val (group, rest) = parse(tail, (0, 0))

          parse(rest, add(acc, multiply(group, factor)))
        } else if (isOpeningBrace(c))
          parse(tail, acc)
        else
          (acc, tail) // closing brace
    }
  }

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val program = io.StdIn.readLine.toList

      val ((rInc, cInc), _) = parse(program)

      println(s"Case #$t: ${cInc + 1} ${rInc + 1}")

    }
  }


  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }

}
