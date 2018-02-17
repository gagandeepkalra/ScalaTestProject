object WeekOfCode36 {

  case class Point(x: Int, y: Int) {

    def distance(a: Point) = math.hypot(x - a.x, y - a.y)

    def add(a: Point) = Point(a.x + x, a.y + y)
  }

  def waysToGiveACheck(board: Array[Array[Char]]): Int = {

    var blackKing: Point = Point(0, 0)
    var whiteKing: Point = Point(0, 0)

    for (i <- 0 to 7; j <- 0 to 7) {
      board(i)(j) match {
        case 'k' => blackKing = Point(i, j)
        case 'K' => whiteKing = Point(i, j)
        case _ =>
      }
    }

    def ifBlackKingIsChecked(board: Array[Array[Char]]): Boolean = {
      (0 to 7).map(i => (0 to 7).map(j =>

        board(i)(j) match {
          // pieces of white
          case 'K' => false
          case 'Q' => checkBishopIsGivingCheck(board, Point(i, j), blackKing) || checkRookIsGivingCheck(board, Point(i, j), blackKing) // queen
          case 'N' => checkKnightIsGivingCheck(board, Point(i, j), blackKing) // knight
          case 'B' => checkBishopIsGivingCheck(board, Point(i, j), blackKing) // bishop
          case 'R' => checkRookIsGivingCheck(board, Point(i, j), blackKing) // rook
          case 'P' => false
          case '#' => false
          case _ => false
        }
      ).reduce[Boolean]((a, b) => a || b)).reduce[Boolean]((a, b) => a || b)

    }

    def ifWhiteKingIsChecked(board: Array[Array[Char]]): Boolean = {
      (0 to 7).map(i => (0 to 7).map(j =>

        board(i)(j) match {
          // pieces of black
          case 'k' => false
          case 'q' => checkBishopIsGivingCheck(board, Point(i, j), whiteKing) || checkRookIsGivingCheck(board, Point(i, j), whiteKing) // queen
          case 'n' => checkKnightIsGivingCheck(board, Point(i, j), whiteKing) // knight
          case 'b' => checkBishopIsGivingCheck(board, Point(i, j), whiteKing) // bishop
          case 'r' => checkRookIsGivingCheck(board, Point(i, j), whiteKing) // rook
          case 'p' => false
          case '#' => false
          case _ => false
        }
      ).reduce[Boolean]((a, b) => a || b)).reduce[Boolean]((a, b) => a || b)

    }

    def checkRookIsGivingCheck(board: Array[Array[Char]], start: Point, dest: Point): Boolean = {

      if (start.x == dest.x || start.y == dest.y) { // same row or column

        val direction: Point = Point(Math.signum(dest.x - start.x).toInt, Math.signum(dest.y - start.y).toInt)
        var temp = start.add(direction)

        while (temp != dest) {
          if (board(temp.x)(temp.y) != '#') return false
          temp = temp.add(direction)
        }
        true
      } else false
    }

    def checkKnightIsGivingCheck(board: Array[Array[Char]], start: Point, dest: Point): Boolean = {
      math.abs(dest.x - start.x) == 2 && math.abs(dest.y - start.y) == 1 ||
        math.abs(dest.x - start.x) == 1 && math.abs(dest.y - start.y) == 2
    }

    def checkBishopIsGivingCheck(board: Array[Array[Char]], start: Point, dest: Point): Boolean = {
      if (math.abs(dest.x - start.x) == math.abs(dest.y - start.y)) {

        val direction: Point = Point(Math.signum(dest.x - start.x).toInt, Math.signum(dest.y - start.y).toInt)
        var temp = start.add(direction)

        while (temp != dest) {
          if (board(temp.x)(temp.y) != '#') return false
          temp = temp.add(direction)
        }
        true

      } else false
    }

    (0 to 7).filter(j => board(1)(j) == 'P' && board(0)(j) == '#').map(j => {

      board(1)(j) = '#'
      var result: Int = 0

      if (!ifWhiteKingIsChecked(board)) {

        if (ifBlackKingIsChecked(board)) {
          result += 4
        } else {
          result += Array('Q', 'N', 'B', 'R').map {
            case 'Q' => checkBishopIsGivingCheck(board, Point(0, j), blackKing) || checkRookIsGivingCheck(board, Point(0, j), blackKing)
            case 'N' => checkKnightIsGivingCheck(board, Point(0, j), blackKing)
            case 'B' => checkBishopIsGivingCheck(board, Point(0, j), blackKing)
            case 'R' => checkRookIsGivingCheck(board, Point(0, j), blackKing)
            case _ => false
          }.count(_ == true)
        }
      }
      board(1)(j) = 'P'

      result
    }).sum
  }

  def chessInput(args: Array[String]) {
    (1 to io.StdIn.readInt()).foreach {
      _ =>
        var board = Array.ofDim[Char](8, 8)
        for (i <- 0 to 8 - 1) {
          io.StdIn.readLine.toCharArray.zipWithIndex.foreach {
            case (c, j) => board(i)(j) = c
          }
        }
        val result = waysToGiveACheck(board)
        println(result)
    }
  }


  def main(args: Array[String]): Unit = {
    // https://www.hackerrank.com/contests/w36/challenges/a-race-against-time
    val n: Int = io.StdIn.readInt
    val h: Int = io.StdIn.readInt

    val height: Array[Long] = io.StdIn.readLine().split(" ").map(_.toLong)
    val price: Array[Long] = io.StdIn.readLine().split(" ").map(_.toLong)

    val map = collection.mutable.Map[(Int, Long), Long]()

    def cost(i: Int, current_height: Long): Long = {
      if (i == n - 1) 1l
      else {
        var x: Long = math.abs(current_height - height(i)) + price(i) + 1
        var t = i + 1

        while (t < n - 1 && height(t - 1) < height(t)) { // t == n-2
          x += math.abs(height(t - 1) - height(t)) + price(t) + 1
          t += 1
        }

        if (map.contains((t, height(t - 1)))) x += map((t, height(t - 1))) else x += cost(t, height(t - 1))

        if (height(i) > current_height) {
          map((i, current_height)) = x
          return x
        }


        var y: Long = 1
        if (map.contains((i + 1, current_height))) y += map((i + 1, current_height)) else y += cost(i + 1, current_height)

        // save result
        map((i, current_height)) = math.min(x, y)

        math.min(x, y)
      }

    }

    println(cost(0, h))
  }
}
