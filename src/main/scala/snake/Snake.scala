package snake

object Snake {

  trait Direction{
    def offset: Vector2Int
  }
  case object Right extends Direction {
    override def offset: Vector2Int = Vector2Int(0, 1)
  }
  case object Left extends Direction {
    override def offset: Vector2Int = Vector2Int(0, -1)
  }
  case object Top extends Direction {
    override def offset: Vector2Int = Vector2Int(-1, 0)
  }
  case object Bottom extends Direction {
    override def offset: Vector2Int = Vector2Int(1, 0)
  }

  case class Vector2Int(x: Int, y: Int) {
    def add(vector2Int: Vector2Int): Vector2Int = Vector2Int(x + vector2Int.x, y + vector2Int.y)
  }

  case class GameState(
      snake: List[Vector2Int],
      food: Vector2Int,
      boardSize: Vector2Int,
      score: Int,
      direction: Direction
  )

  def next(state: GameState): GameState = {
    state.copy(
      snake = updateSnakePosition(snake = state.snake, direction = state.direction)
    )
  }

  def changeDirection(state: GameState, direction: Direction): GameState = {
    state.copy(direction = direction)
  }

  def updateSnakePosition(snake: List[Vector2Int], direction: Direction, head: Boolean = true): List[Vector2Int] = {
    snake.head.add(direction.offset) +: snake.dropRight(1)
  }
}
