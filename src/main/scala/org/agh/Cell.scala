package org.agh

import java.awt.Color
import java.awt.Color._

case class Cell(x: Int, y: Int, value: Color) {
  def apply(f: (Int, Int) => Option[Color]): Cell = {
    val value = f(x, y)

    value match {
      case Some(color) => Cell(x, y, color)
      case None => this
    }
  }
}

object Cell {
  def apply(x: Int, y: Int): Cell = {
    Cell(x, y, WHITE)
  }
}
