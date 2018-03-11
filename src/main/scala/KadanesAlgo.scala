object KadanesAlgo {

  // maximum sum contagious sub-array

  def main(args: Array[String]): Unit = {
    val n = io.StdIn.readInt
    val arr = io.StdIn.readLine.split(" ").map(_.toLong)

    var max: Long = 0
    var sum: Long = 0
    arr.indices.foreach(i => {
      sum += arr(i)
      if (sum < 0) sum = 0
      else if (sum > max) max = sum
    })

    println(max)
  }

}
