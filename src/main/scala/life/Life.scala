package life

object Life {
  case class GameState(board: Array[Array[Int]], size: (Int, Int));

  def init(width: Int, height: Int): GameState = {
    GameState(Array.fill(width, height)(0), (width, height))
  }

  def next(state: GameState): GameState =
    state.copy(board = state.board.zipWithIndex.map {
      case (xel, x) =>
        xel.zipWithIndex.map {
          case (_, y) => {
            countAliveAround(state, (x, y)) match {
              case n if n == 3 => 1
              case n if n == 2 => state.board(x)(y)
              case _           => 0
            }
          }
        }
    })

  def getPosAround(state: GameState, pos: (Int, Int)): Seq[(Int, Int)] =
    for {
      x <- pos._1 - 1 to pos._1 + 1
      y <- pos._2 - 1 to pos._2 + 1 if (x != pos._1 || y != pos._2)
    } yield (x, y)


  def countAliveAround(state: GameState, pos: (Int, Int)): Int = getPosAround(state, pos).map(getCellType(state, _)).count(_ == 1)

  def getCellType(state: GameState, pos: (Int, Int)): Int = pos match {
    case p
        if p._1 <= -1 | p._2 <= -1 | p._1 >= state.size._1 | p._2 >= state.size._2 =>
      0
    case _ => state.board(pos._1)(pos._2)
  }
}
