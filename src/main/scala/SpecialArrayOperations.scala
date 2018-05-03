import scala.collection.mutable

/**
  * You are given an array A of size N. You can perform an operation in which you will remove the largest and the
  * smallest element from the array and add  their difference back into the array. So, the size of the array will
  * decrease by 1 after each operation. You are given Q tasks and in each task, you are given an integer K. For
  * each task, you have to tell sum of all the elements in the array after K operations.
  */
object SpecialArrayOperations {

  def main(args: Array[String]): Unit = {

    val Array(n, q) = io.StdIn.readLine.split(" ").map(_.toInt)

    val frequency = collection.mutable.Map[Int, Int]()
    val maxHeap = mutable.PriorityQueue[Int]()
    val minHeap = mutable.PriorityQueue[Int]()(Ordering.by(-_))

    def getElementFrom(store: mutable.PriorityQueue[Int]): Int = {
      val element = store.dequeue()
      val frequencyOfCurrentElement = frequency(element)

      if (frequencyOfCurrentElement == 0) getElementFrom(store) else {
        frequency(element) = frequencyOfCurrentElement - 1
        element
      }
    }

    val result = new Array[Long](n)

    result(0) = io.StdIn.readLine().split(" ").map(t => {
      val x = t.toInt
      frequency(x) = frequency.getOrElse(x, 0) + 1
      maxHeap.enqueue(x)
      minHeap.enqueue(x)

      x.toLong
    }).sum

    (1 until n).foreach(k => {
      val maxElement = getElementFrom(maxHeap)
      val minElement = getElementFrom(minHeap)

      val diff = maxElement - minElement

      maxHeap.enqueue(diff)
      minHeap.enqueue(diff)

      frequency(diff) = frequency.getOrElse(diff, 0) + 1
      result(k) = result(k - 1) - 2l * minElement.toLong
    })

    (1 to q).foreach(_ => {
      println(result(io.StdIn.readInt))
    })

  }
}
