package utils

import java.io.IOException

import org.jline.terminal.TerminalBuilder

class Terminal {
  val reader = TerminalBuilder.terminal().reader()

  def getKeyInput: Option[Int] = {
    try{
      Some(reader.read(1))
    }catch{
      case _: IOException => None
    }
  }

  def clear: Unit = println("\u001b[2J")
}
