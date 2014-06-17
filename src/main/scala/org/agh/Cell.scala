package org.agh

import java.awt.Color
import java.awt.Color._

import scala.collection.Seq

case class Cell(x: Int, y: Int,
                value: Color,
                energy: Double = 0,
                recrystallized: Boolean = false) extends Energy {

  val active = value match {
    case BLACK => false
    case _ => true
  }

  val permanent = value match {
    case BLACK | WHITE => true
    case _ => false
  }

  def apply(f: Cell => Option[Color]): Cell = {
    val value = f(this)

    value match {
      case Some(color) => Cell(x, y, color)
      case None => this
    }
  }

  def applyMC(states: Seq[Color]): Cell = {
    val beforeState = value
    val afterState = RANDOM.shuffle(states).head
    val beforeEnergy = energy(states, beforeState)
    val afterEnergy = energy(states, afterState)

    if (afterEnergy - beforeEnergy <= 0) {
      Cell(x, y, afterState)
    } else {
      this
    }
  }

  def applySRX(states: Seq[Cell]): Cell = {
    if (states.nonEmpty) {
      val afterState = RANDOM.shuffle(states).head
      val beforeEnergy = (energy(states, this) * 0.5) + energy
      val afterEnergy = energy(states, afterState) * 0.5

      if (afterEnergy - beforeEnergy <= 0) afterState ~ true else this

    } else this
  }

  def +(eng: Double): Cell = Cell(x, y, value, energy + eng, recrystallized)

  def -(eng: Double): Cell = Cell(x, y, value, energy - eng, recrystallized)

  def ~(c: Color): Cell = Cell(x, y, c, energy, recrystallized)

  def ~(b: Boolean): Cell = Cell(x, y, value, energy, b)

  def ~(x: Int, y: Int): Cell = Cell(x, y, value, energy, recrystallized)

}

object Cell {
  def apply(x: Int, y: Int): Cell = {
    Cell(x, y, WHITE)
  }
}

trait Energy {
  def energy(s: Seq[Color], c: Color): Double = {
    s.map {
      case `c` => 0.0
      case _ => 1.0
    }.sum
  }

  def energy(s: Seq[Cell], c: Cell): Double = {
    val state = s.filterNot(_.recrystallized).map(_.value)
    energy(state, c)
  }
}
