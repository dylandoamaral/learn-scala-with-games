package snake

import snake.Snake.GameState
import utils.Terminal

object Main extends App {
  val terminal: Terminal = new Terminal;

  val init: GameState = GameState(
    snake = List(Snake.Vector2Int(2, 2), Snake.Vector2Int(2, 3)),
    food = Snake.Vector2Int(4, 4),
    boardSize = Snake.Vector2Int(10, 20),
    score = 0,
    direction = Snake.Left
  )

  val keyMapping: Map[Int, String] =
    Map(113 -> "left", 100 -> "right", 122 -> "top", 115 -> "bottom", 114 -> "restart")

  /**
    * The main game loop
    * Doesn't work with the intelliJ console, please use SBT instead
    * @param state The current state of the game
    */
  @scala.annotation.tailrec
  def printGameState(state: GameState): Unit = {
    terminal.clear
    println("score : " + state.score)

    for (x <- 0 until state.boardSize.x) {
      for (y <- 0 until state.boardSize.y) {
        Snake.Vector2Int(x, y) match {
          case v if state.snake.contains(v) => print("O")
          case v if v == state.food         => print("•")
          case _                            => print("-")
        }
      }
      println
    }

    Thread.sleep(200)

    terminal.getKeyInput match {
      case Some(k) if keyMapping contains k =>
        keyMapping(k) match {
          case v if v == "left"    => printGameState(Snake.next(state.copy(direction = Snake.Left)))
          case v if v == "right"   => printGameState(Snake.next(state.copy(direction = Snake.Right)))
          case v if v == "top"     => printGameState(Snake.next(state.copy(direction = Snake.Top)))
          case v if v == "bottom"  => printGameState(Snake.next(state.copy(direction = Snake.Bottom)))
          case v if v == "restart" => printGameState(init)
          case _                   => printGameState(Snake.next(state))
        }

      case _ => printGameState(Snake.next(state))
    }
  }

  printGameState(init)
}
