object DicePath {

  case class Dice(top: Int, bottom: Int, left: Int, right: Int, front: Int, back: Int) {
    def moveDown = Dice(back, front, left, right, top, bottom)

    def moveRight = Dice(left, right, bottom, top, front, back)
  }

  case class Path(result: Map[Dice, Int]) {

    def moveDown =
      Path(result map { case (dice: Dice, value: Int) => (dice.moveDown, value + dice.back) })

    def moveRight =
      Path(result map { case (dice: Dice, value: Int) => (dice.moveRight, value + dice.left) })

    def merge(path: Path) =
      Path((result.keySet ++ path.result.keySet).map(dice => (dice, math.max(path.result.getOrElse(dice, 0), result.getOrElse(dice, 0)))).toMap)
  }

  def main(args: Array[String]): Unit = {

    val board = Array.ofDim[Path](61, 61)

    board(1)(1) = Path(Map(Dice(1, 6, 3, 4, 2, 5) -> 1))

    (1 to 60).foreach(i => (1 to 60).foreach(f = j => board(i)(j) = (i, j) match {
      case (1, 1) => board(i)(j)
      case (1, j) => board(1)(j - 1).moveRight
      case (i, 1) => board(i - 1)(1).moveDown
      case _ => board(i - 1)(j).moveDown.merge(board(i)(j - 1).moveRight)
    }))

    (1 to io.StdIn.readInt).foreach(f = _ => {
      val Array(i, j) = io.StdIn.readLine().split(" ").map(_.toInt)
      println(board(i)(j).result.values.max)
    })
  }
}
