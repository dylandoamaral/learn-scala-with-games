package snake

import scala.util.Random

object Snake {

  val random: Random = Random

  trait Direction {
    def offset: Vector2Int
    def opposite: Direction
  }

  case object Right extends Direction {
    override def offset: Vector2Int = Vector2Int(0, 1)

    override def opposite: Direction = Left
  }
  case object Left extends Direction {
    override def offset: Vector2Int = Vector2Int(0, -1)

    override def opposite: Direction = Right
  }
  case object Top extends Direction {
    override def offset: Vector2Int = Vector2Int(-1, 0)

    override def opposite: Direction = Bottom
  }
  case object Bottom extends Direction {
    override def offset: Vector2Int = Vector2Int(1, 0)

    override def opposite: Direction = Top
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

  val init: GameState = GameState(
    snake = List(Snake.Vector2Int(5, 10), Snake.Vector2Int(5, 11)),
    food = Vector2Int(random.between(0, 10), random.between(0, 20)),
    boardSize = Snake.Vector2Int(10, 20),
    score = 0,
    direction = Snake.Left
  )

  def next(state: GameState): GameState = {
    if(die(state.snake, state.boardSize)){
      init
    }else{
      state.copy(
        snake = updateSnake(snake = state.snake, direction = state.direction, food = state.food),
        food = duringEat(state.snake, state.food)(state.food, generateNewFood(state.snake, state.food, state.boardSize)),
        score = duringEat(state.snake, state.food)(state.score, state.score + 100)
      )
    }
  }

  def changeDirection(state: GameState, direction: Direction): GameState = {
    if (direction == state.direction.opposite) next(state)
    else next(state.copy(direction = direction))
  }

  def updateSnake(snake: List[Vector2Int], direction: Direction, food: Vector2Int): List[Vector2Int] = {
    duringEat(snake, food)(snake.head.add(direction.offset) +: snake.dropRight(1),
                           snake.head.add(direction.offset) +: snake)
  }

  def duringEat[A](snake: List[Vector2Int], food: Vector2Int)(b: A, n: A): A = if (snake contains food) n else b

  @scala.annotation.tailrec
  def generateNewFood(snake: List[Vector2Int], food: Vector2Int, boardSize: Vector2Int): Vector2Int = {
    val newPosition = Vector2Int(random.between(0, boardSize.x), random.between(0, boardSize.y))
    if (snake contains newPosition) { generateNewFood(snake, food, boardSize) } else newPosition
  }

  def die(snake: List[Vector2Int], boardSize: Vector2Int): Boolean = {
    if (snake.distinct.length != snake.length || snake.exists(
          pos => pos.x < 0 || pos.y < 0 || pos.x > boardSize.x || pos.y > boardSize.y)) true
    else false
  }
}
