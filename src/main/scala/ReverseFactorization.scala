

object ReverseFactorization {


  def main(args: Array[String]): Unit = {

    // Input
    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
    val elements = io.StdIn.readLine.split(" ").map(_.toInt).sorted.toList

    val parentMap = collection.mutable.Map[Int, Int]()

    val queue: collection.mutable.Queue[Int] = collection.mutable.Queue[Int]()

    queue.enqueue(1)
    parentMap(1) = 1

    while (queue.nonEmpty && queue.front != n) {
      val x = queue.dequeue()
      val parentMultiplier = parentMap(x)

      elements.filter(i => i >= parentMultiplier && x*i <= n && !parentMap.contains(x * i)).foreach(i => {
        parentMap(x * i) = i
        queue.enqueue(x * i)
      })
    }

    def print_result(x: Int): Unit = {
      if (x != 1) print_result(x / parentMap(x))
      print(x + " ")
    }

    if (queue.isEmpty) println(-1) else print_result(queue.front)
  }

}
