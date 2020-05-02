package google.kickstart._2020.A

/*
https://codingcompetitions.withgoogle.com/kickstart/round/000000000019ffc7/00000000001d3ff3

[Trie]

Insert all prefixes in a trie, then in a depth first manner start bundling (longest prefix first) into groups of k
 */
object Bundling {

  case class Trie(hits: Int, next: Map[Char, Trie]) {
    def insertAllPrefixes(chars: List[Char]): Trie = {
      if (chars.isEmpty)
        this.copy(hits = hits + 1)
      else {
        val c = chars.head
        Trie(hits + 1, next + (c -> next.getOrElse(c, Empty).insertAllPrefixes(chars.tail)))
      }
    }
  }

  object Empty extends Trie(0, Map.empty)


  private def add(l: (Int, Int), r: (Int, Int)): (Int, Int) = (l._1 + r._1, l._2 + r._2)

  def main(args: Array[String]): Unit = {
    for (t <- 1 to io.StdIn.readInt) {
      val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)

      val root = (1 to n).foldLeft[Trie](Trie(0, Map.empty))((trie, _) => trie.insertAllPrefixes(io.StdIn.readLine.toList))

      def calculate(trie: Trie, depth: Int): (Int, Int) = {
        val (hitsConsumed, result) = trie.next.foldLeft((0, 0))((acc, kv) => add(acc, calculate(kv._2, depth + 1)))

        val remainingHits = trie.hits - hitsConsumed
        (hitsConsumed + remainingHits - remainingHits % k, depth * (remainingHits / k) + result)
      }

      printFormattedOutput(t, calculate(root, 0)._2)
    }
  }

  def printFormattedOutput(i: Int, op: AnyVal): Unit = {
    println(s"Case #$i: $op")
  }
}
