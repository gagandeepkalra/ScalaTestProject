package google.kickstart._2020.E

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ff47/00000000003bede9

Goal is for each ri - sum e + ei >= 0

We simulate the second cycle, try include for each i, stabilize by removing the max e + r values and then update max after each stabilization
 */
object Toys {

  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val n = io.StdIn.readInt()
      val seq = (0 until n).map(_ => io.StdIn.readLine.split(" ").map(_.toInt))

      val totalE = seq.foldLeft(0L)(_ + _ (0))

      def score(i: Int): Long = seq(i)(0) + seq(i)(1)

      val priorityQueue: mutable.PriorityQueue[Int] = collection.mutable.PriorityQueue()(Ordering.by(score))

      @scala.annotation.tailrec
      def loop(i: Int = 0, accSum: Long = totalE, accCurrTime: Long = totalE, accRemoved: Int = 0, result: (Int, Long) = (0, totalE)): Unit = {
        if (i == n)
          printFormattedOutput(t, if (priorityQueue.nonEmpty) s"$accRemoved INDEFINITELY" else s"${result._1} ${result._2}")
        else {
          priorityQueue.enqueue(i)

          var sum: Long = accSum
          var currTime: Long = accCurrTime + seq(i)(0)
          var removed: Int = accRemoved

          // remove while r+e  >= sum
          while (priorityQueue.headOption.exists(score(_) > sum)) {
            val j = priorityQueue.dequeue()
            sum -= seq(j)(0)
            currTime -= seq(j)(0) * 2
            removed += 1
          }

          val resultU = if (currTime > result._2) (removed, currTime) else result

          loop(i + 1, sum, currTime, removed, resultU)
        }
      }

      loop()
    }
  }

  def printFormattedOutput(i: Int, op: Any): Unit = {
    println(s"Case #$i: $op")
  }
}
