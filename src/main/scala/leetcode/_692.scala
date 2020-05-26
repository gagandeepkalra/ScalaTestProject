package leetcode

/*
https://leetcode.com/problems/top-k-frequent-words/
 */
object _692 {

  case class Freq(s: String, ct: Int)

  object Freq {
    implicit val ordering: Ordering[Freq] = (x: Freq, y: Freq) =>
      if (x.ct == y.ct) scala.math.Ordering.String.compare(x.s, y.s) * -1 else x.ct - y.ct
  }

  def topKFrequent(words: Array[String], k: Int): List[String] = {
    val mp = scala.collection.mutable.Map[String, Int]()
    words.foreach { x => mp.put(x, mp.getOrElse(x, 0) + 1) }

    val pq = scala.collection.mutable.PriorityQueue[Freq](mp.map { case (k, v) => Freq(k, v) }.toList: _*)

    (1 to k).foldLeft(List.empty[String])((acc, _) => if (pq.nonEmpty) pq.dequeue().s :: acc else acc).reverse
  }

  def main(args: Array[String]): Unit = {
    val ls = Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is", "is", "for", "for", "for", "sunny", "for")

    println {
      topKFrequent(ls, 5)
    }
  }

}
