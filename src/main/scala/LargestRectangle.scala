object LargestRectangle {

  def main(args: Array[String]): Unit = {
    val n: Int = io.StdIn.readInt
    val arr = io.StdIn.readLine().split(" ").map(_.toInt)

    val left = new Array[Int](n)
    val right = new Array[Int](n)

    val stack = scala.collection.mutable.Stack[Int]() // stores indices

    (0 until n).foreach(i => {
      while (!stack.isEmpty && arr(stack.top) >= arr(i)) stack.pop()
      left(i) = if (stack.isEmpty) -1 else stack.top
      stack.push(i)
    })

    stack.clear()

    (n - 1 to 0 by -1).foreach(i => {
      while (!stack.isEmpty && arr(stack.top) >= arr(i)) stack.pop()
      right(i) = if (stack.isEmpty) n else stack.top
      stack.push(i)
    })

    println((0 until n).foldLeft[Int](Int.MinValue)((res, i) => {
      math.max(res, (right(i) - left(i) - 1) * arr(i))
    }))

  }
}
