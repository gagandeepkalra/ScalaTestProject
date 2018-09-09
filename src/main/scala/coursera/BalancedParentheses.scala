package coursera

object BalancedParentheses {

  def isBalanced(source: Array[Char]): Boolean = {

    def balanced(idx: Int, count: Int): Boolean = {
      if (idx == source.length) count >= 0
      else if (count < 0) false
      else source(idx) match {
        case '(' => balanced(idx + 1, count + 1)
        case ')' => balanced(idx + 1, count - 1)
        case _ => balanced(idx + 1, count)
      }
    }

    balanced(0, 0)
  }

  def main(args: Array[String]): Unit = {
    assert(!isBalanced("((())))".toArray))
    assert(isBalanced("((()))".toArray))
    assert(isBalanced("((a))(bc)sww(w(s(e)re)e)w__q".toArray))
  }
}
