object PuzzleAndPC {
  def main(args: Array[String]): Unit = {
    val m = io.StdIn.readInt()
    val n = 1 << m
    val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)

    val result = Array.ofDim[Int](n + 1, n + 1)

    divide(r, c, 1, 1, n, n)

    def divide(r: Int, c: Int, x1: Int, y1: Int, x2: Int, y2: Int): Unit = { // square
      if (x1 != x2 && y1 != y2) {

        val x = (x1 + x2) / 2
        val y = (y1 + y2) / 2

        // Quad 1
        if (notInSquare(x1, y1, x, y)) print(x + " " + y + " ")
        // Quad 2
        if (notInSquare(x1, y + 1, x, y2)) print(x + " " + (y + 1) + " ")
        // Quad 3
        if (notInSquare(x + 1, y1, x2, y)) print((x + 1) + " " + y + " ")
        // Quad 4
        if (notInSquare(x + 1, y + 1, x2, y2)) print((x + 1) + " " + (y + 1) + " ")

        println

        // divide

        // Quad 1
        if (notInSquare(x1, y1, x, y)) divide(x, y, x1, y1, x, y) else divide(r, c, x1, y1, x, y)
        // Quad 2
        if (notInSquare(x1, y + 1, x, y2)) divide(x, y + 1, x1, y + 1, x, y2) else divide(r, c, x1, y + 1, x, y2)
        // Quad 3
        if (notInSquare(x + 1, y1, x2, y)) divide(x + 1, y, x + 1, y1, x2, y) else divide(r, c, x + 1, y1, x2, y)
        // Quad 4
        if (notInSquare(x + 1, y + 1, x2, y2)) divide(x + 1, y + 1, x + 1, y + 1, x2, y2) else divide(r, c, x + 1, y + 1, x2, y2)
      }

      def notInSquare(x1: Int, y1: Int, x2: Int, y2: Int) = !(x1 <= r && r <= x2 && y1 <= c && c <= y2)
    }

  }
}
