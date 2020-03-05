package life

import life.Life.GameState
import utils.Terminal

object Main extends App {
  val terminal: Terminal = new Terminal;

  val blinker = GameState(
    Array(
      Array(0, 0, 0),
      Array(1, 1, 1),
      Array(0, 0, 0)
    ),
    (3, 3)
  )

  val block = GameState(
    Array(
      Array(0, 0, 0, 0),
      Array(0, 1, 1, 0),
      Array(0, 1, 1, 0),
      Array(0, 0, 0, 0)
    ),
    (4, 4)
  )

  val glider = GameState(
    Array(
      Array(0, 0, 0, 1, 0),
      Array(0, 0, 1, 0, 0),
      Array(0, 0, 1, 1, 1),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0)
    ),
    (5, 5)
  )

  val structureMapping: Map[Int, GameState] =
    Map(97 -> blinker, 122 -> block, 101 -> glider)

  /**
    * The main game loop
    * Doesn't work with the intelliJ console, please use SBT instead
    * @param state The current state of the game
    */
  @scala.annotation.tailrec
  def printGameState(state: GameState): Unit = {
    terminal.clear
    state.board.foreach(
      line => {
        line.foreach {
          case 0 => print("0")
          case _ => print("1")
        }
        println()
      }
    )

    Thread.sleep(1000)

    terminal.getKeyInput match {
      case Some(k) => printGameState(structureMapping(k))
      case None    => printGameState(Life.next(state))
    }
  }

  printGameState(blinker)
}
