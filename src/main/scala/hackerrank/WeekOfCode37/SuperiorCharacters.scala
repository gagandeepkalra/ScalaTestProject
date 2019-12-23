package hackerrank.WeekOfCode37

object SuperiorCharacters {

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(_ => {
      val arr: Array[Long] = io.StdIn.readLine.split(" ").map(_.toLong)

      val count: Long = arr.sum // using 1 based indexing

      val maxResult: Long = (count - 1) / 2
      val medianIndex: Long = count - maxResult + 1

      var leftCount: Long = 0
      var i = 0

      while (leftCount + arr(i) < medianIndex) {
        leftCount += arr(i)
        i += 1
      }

      val rightCount = leftCount + arr(i) - medianIndex + 1


      if (leftCount > rightCount) {
        println(maxResult)
      } else {
        println(maxResult - rightCount + leftCount - 1)
      }

    })
  }
}
