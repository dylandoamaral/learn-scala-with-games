package life

object Life {
  sealed trait CellType
  case object Alive extends CellType
  case object Dead  extends CellType

  case class GameState(board: Array[Array[CellType]], size: (Int, Int));

  def init(width: Int, height: Int): GameState = {
    GameState(Array.fill(width, height)(Dead), (width, height))
  }

  // TODO refractor this horrible function
  def next(state: GameState): GameState =
    GameState(
      state.board.zipWithIndex.map {
        case (xel, x) =>
          xel.zipWithIndex.map {
            case (_, y) => {
              getAliveAround(state, (x, y)) match {
                case n if n == 3 => Alive
                case n if n == 2 => state.board(x)(y)
                case _           => Dead
              }
            }
          }
      },
      state.size
    )

  // TODO refractor this horrible function
  def getAliveAround(state: GameState, pos: (Int, Int)): Int = {
    (getCellType(state, (pos._1 - 1, pos._2 - 1)) ::
      getCellType(state, (pos._1, pos._2 - 1)) ::
      getCellType(state, (pos._1 - 1, pos._2)) ::
      getCellType(state, (pos._1 + 1, pos._2 + 1)) ::
      getCellType(state, (pos._1, pos._2 + 1)) ::
      getCellType(state, (pos._1 + 1, pos._2)) ::
      getCellType(state, (pos._1 + 1, pos._2 - 1)) ::
      getCellType(state, (pos._1 - 1, pos._2 + 1)) :: Nil).count(_ == Alive)
  }

  def getCellType(state: GameState, pos: (Int, Int)): CellType = pos match {
    case p
        if p._1 <= -1 | p._2 <= -1 | p._1 >= state.size._1 | p._2 >= state.size._2 =>
      Dead
    case _ => state.board(pos._1)(pos._2)
  }
}
