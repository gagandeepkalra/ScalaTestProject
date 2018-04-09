package coursera

object BalancedParentheses {

  def isBalanced(source: Array[Char]): Boolean = {

    def checkIfBalanced(i: Int): Int = { // returns index of right parentheses corresponding to i
      if (i >= source.length || source(i) == ')') -1
      else {
        var ii = i + 1
        while (ii < source.length && source(ii) == '(') {
          ii = checkIfBalanced(ii)
          if (ii == -1) return -1

          ii += 1
        }
        if (ii < source.length && source(ii) == ')') ii else -1
      }
    }

    var i = 0
    while (i < source.length) { // testing cases like: ()()(())
      i = checkIfBalanced(i)
      if (i == -1) return false else i += 1
    }
    true

  }
}
