//def missingWords(s: String, t: String): Array[String] = {
//
//  val subSet = t.split(" ").toList
//
//  val superSet = s.split(" ").toList
//
//  def sequenceDiff(superSet: scala.collection.immutable.List[String], subSet: scala.collection.immutable.List[String]): scala.collection.immutable.List[String] = {
//    if (subSet.isEmpty) superSet
//    else {
//      superSet.takeWhile(_ != subSet.head) ++ sequenceDiff(superSet.dropWhile(_ != subSet.head).tail, subSet.tail)
//    }
//  }
//
//  sequenceDiff(superSet, subSet).toArray
//
//}


def kSub(k: Int, nums: Array[Int]): Long = {

  val prefixSum = nums.scanLeft(0)((acc, i) => (acc + i) % k).tail

  prefixSum
    .groupBy(identity)
    .map { case (value, copies) => copies.length.toLong }
    .foldLeft(0l) { case (acc, freq) => acc + (if (freq > 1) freq * (freq - 1) / 2 else 0) } + prefixSum.count(_ == 0)


}

kSub(5, Array(5, 10, 11, 9, 5))