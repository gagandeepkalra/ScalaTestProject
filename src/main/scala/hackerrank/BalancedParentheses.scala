package hackerrank

/*
Return if the given string is balanced
 */
object BalancedParentheses {

  def braces(values: Array[String]): Array[String] = {
    @scala.annotation.tailrec
    def isBalanced(ls: List[Char], stack: List[Char] = Nil): Boolean = {
      if (ls.isEmpty) stack.isEmpty else {
        ls.head match {
          case '(' | '{' | '[' => isBalanced(ls.tail, ls.head :: stack)
          case ')' => if (stack.headOption.contains('(')) isBalanced(ls.tail, stack.tail) else false
          case '}' => if (stack.headOption.contains('{')) isBalanced(ls.tail, stack.tail) else false
          case ']' => if (stack.headOption.contains('[')) isBalanced(ls.tail, stack.tail) else false
        }
      }
    }

    values.map(s => if(isBalanced(s.toList)) "YES" else "NO")
  }


  def main(args: Array[String]): Unit = {
    braces(Array("{}[]()", "{[}]}", "{[()]}", "{[()}]")).foreach(println)
  }
}
