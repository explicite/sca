package org.agh

import java.awt.Color._
import java.awt.Color

case class Cell(x: Int, y: Int, value: Color) {
  def apply(f: (Int, Int) => Option[Color]): Cell = {
    val value = f(x, y)

    value match {
      case Some(color) => Cell(x, y, color)
      case None => this
    }
  }

  def energy(f: (Int, Int) => Seq[Color]): Int = {
    energy(f(x, y))
  }

  def energy(s: Seq[Color]):Int = {
    s.map {
      case `value` => 0
      case _ => 1
    }.sum
  }
}

object Cell {
  def apply(x: Int, y: Int): Cell = {
    Cell(x, y, WHITE)
  }
}
