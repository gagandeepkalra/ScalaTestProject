object SegmentTreesRMQ {

  def heightOfSegmentTree(n: Int): Int = {
    2 * Math.pow(2, Math.ceil(Math.log(n) / Math.log(2)).toInt).toInt - 1
  }

  def main(args: Array[String]): Unit = {

    val Array(n, k) = io.StdIn.readLine.split(" ").map(_.toInt)
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)
    val segmentArr = new Array[Int](heightOfSegmentTree(n))

    def preCompute(l: Int, r: Int, i: Int): Unit = {
      if (l == r) {
        segmentArr(i) = arr(l)
      }
      else {
        preCompute(l, (l + r) / 2, 2 * i + 1)
        preCompute((l + r) / 2 + 1, r, 2 * i + 2)
        segmentArr(i) = math.min(segmentArr(2 * i + 1), segmentArr(2 * i + 2))
      }
    }

    def query(l: Int, r: Int, i: Int, x: Int, y: Int): Int = {
      if (r < x || l > y || x > y || l > r) Int.MaxValue
      else if (l == x && r == y) segmentArr(i)
      else {
        val m = (l + r) / 2
        if (x <= m && m <= y)
          math.min(query(l, m, 2 * i + 1, x, m), query(m + 1, r, 2 * i + 2, m + 1, y))
        else if (y < m) query(l, m, 2 * i + 1, x, y)
        else if (m < x) query(m+1, r, 2 * i + 2, x, y)
        else Int.MaxValue
      }
    }

    preCompute(0, n - 1, 0)

    (1 to k).foreach(_ => {
      println((query _).tupled(io.StdIn.readLine.split(" ").map(_.toInt) match {
        case Array(x, y) => (0, n - 1, 0, x, y)
        case _ => (0, n - 1, 0, 0, 0)
      }))
    })
  }

}
