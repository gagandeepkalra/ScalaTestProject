
object SubsetSum {

  def main(args: Array[String]): Unit = {


    val n = readLine().toInt
    val arr: Array[Long] = readLine().split(" ").map(_.toLong)

    scala.util.Sorting.quickSort(arr)

    (n - 2 to 0 by -1).foreach {
      i: Int => {
        arr(i) += arr(i + 1)
      }
    }

    (1 to readLine().toInt).foreach(_ => {
      println(binarySearch(arr, readLine().toLong, 0, n - 1))
    })


  }

  def binarySearch(arr: Array[Long], key: Long, x: Int, y: Int): Int = {
    val mid = (x + y) / 2

    if (x > y) return -1

    if (mid + 1 == arr.size || arr(mid + 1) < key && key <= arr(mid)) return arr.size - mid

    if (key < arr(mid)) binarySearch(arr, key, mid + 1, y) else binarySearch(arr, key, x, mid - 1)
  }


}
