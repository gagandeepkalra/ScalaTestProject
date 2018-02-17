object Powers {

  def numberOfWays(X: Int, N: Int): Int = {

    val limit = math.pow(X, 1.0 / N).floor.toInt

    def count(x: Int, i: Int): Int = {
      if (i > limit || x > X) 0
      else if (x + math.pow(i, N).toInt == X) 1
      else count(x + math.pow(i, N).toInt, i + 1) + count(x, i + 1)
    }

    count(0, 1)
  }

}
