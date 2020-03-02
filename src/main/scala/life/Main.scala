package life

import life.Life.{Alive, Dead, GameState}

object Main extends App {
  def printGameState(state: GameState): Unit = {
    state.board.foreach(
      line => {
        line.foreach {
          case Dead => print("0")
          case _    => print("1")
        }
        println()
      }
    )

    Thread.sleep(1000)
    printGameState(Life.next(state))
  }

  val state = GameState(
    Array(
      Array(Dead, Dead, Dead),
      Array(Alive, Alive, Alive),
      Array(Dead, Dead, Dead)
    ), (3, 3)
  )


  printGameState(state)
}
