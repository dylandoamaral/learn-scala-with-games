package life

import java.io.IOException

import life.Life.GameState
import org.jline.terminal.TerminalBuilder

object Main extends App {
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

  val glider = GameState(
    Array(
      Array(0, 0, 0, 1, 0),
      Array(0, 0, 1, 0, 0),
      Array(0, 0, 1, 1, 1),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0)
    ), (5, 5)
  )

  val structureMapping: Map[Int, GameState] = Map(97 -> blinker, 122 -> block, 101 -> glider)

  @scala.annotation.tailrec
  def printGameState(state: GameState): Unit = {
    println("\u001b[2J")
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

    getKeyInput match {
      case Some(k) => printGameState(structureMapping(k))
      case None => printGameState(Life.next(state))
    }
  }

  val reader = TerminalBuilder.terminal().reader()

  def getKeyInput: Option[Int] = {
    try{
      reader.read(1) match {
        case v if structureMapping.exists(_._1 == v) => Some(v)
        case _ => None
      }
    }catch{
      case _: Throwable => None
    }
  }

  printGameState(blinker)
}
