package life

import life.Life.GameState

object Main extends App {
  def printGameState(state: GameState): Unit = {
    state.board.foreach(
      line => {
        line.foreach {
          case 0 => print("0")
          case _    => print("1")
        }
        println()
      }
    )

    Thread.sleep(1000)

    printGameState(Life.next(state))
  }

  val blinker = GameState(
    Array(
      Array(0, 0, 0),
      Array(1, 1, 1),
      Array(0, 0, 0)
    ), (3, 3)
  )

  val block = GameState(
    Array(
      Array(0, 0, 0, 0),
      Array(0, 1, 1, 0),
      Array(0, 1, 1, 0),
      Array(0, 0, 0, 0)
    ), (4, 4)
  )


  printGameState(block)
}
