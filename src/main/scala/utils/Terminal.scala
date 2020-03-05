package utils

import java.io.IOException

import life.Main.structureMapping
import org.jline.terminal.TerminalBuilder

class Terminal {
  val reader = TerminalBuilder.terminal().reader()

  def getKeyInput: Option[Int] = {
    try{
      reader.read(1) match {
        case v if structureMapping.exists(_._1 == v) => Some(v)
        case _ => None
      }
    }catch{
      case _: IOException => None
    }
  }

  def clear: Unit = println("\u001b[2J")
}
