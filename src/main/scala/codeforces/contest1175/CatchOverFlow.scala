package codeforces.contest1175

/*
https://codeforces.com/contest/1175/problem/B

Context sensitive parser
 */
object CatchOverFlow {

  type Statements = List[List[String]]

  def main(args: Array[String]): Unit = {
    val lines = io.StdIn.readInt()


    countIncrements((1 to lines).map(_ => io.StdIn.readLine().split(" ").toList).toList, 0) match {
      case Some((result, Nil)) => println(result)
      case None => println("OVERFLOW!!!")
    }

    def countIncrements(statements: Statements, result: Long): Option[(Long, Statements)] = {
      if (result > (1l << 32) - 1)
        None
      else
        statements match {
          case Nil => Some(result, Nil)
          case statement :: rest =>

            statement match {
              case "for" :: n :: Nil =>
                countIncrements(rest, 0).flatMap {
                  case (forLoopResult, statementsAfterForLoop) =>
                    countIncrements(statementsAfterForLoop, result + forLoopResult * n.toInt)
                }

              case "end" :: Nil =>
                Some(result, rest)

              case "add" :: Nil =>
                countIncrements(rest, result + 1)
            }
        }
    }
  }

}
