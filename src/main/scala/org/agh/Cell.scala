package org.agh

import java.awt.Color
import java.awt.Color._

import scala.collection.Seq

case class Cell(x: Int, y: Int,
                value: Color,
                energy: Double = 0) extends Energy {

  val recrystallized = value match {
    case WHITE => false
    case _ => true
  }

  val active = value match {
    case BLACK => false
    case _ => true
  }

  val permanent = value match {
    case BLACK | WHITE => true
    case _ => false
  }

  def apply(f: (Int, Int) => Option[Color]): Cell = {
    val value = f(x, y)

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

  def applySRX(states: Seq[Color]): Cell = {
    val beforeState = value
    val afterState = RANDOM.shuffle(states).head
    val beforeEnergy = (energy(states, beforeState) * 0.5) + energy
    val afterEnergy = energy(states, afterState) * 0.5

    if (afterEnergy - beforeEnergy <= 0) {
      Cell(x, y, afterState)
    } else {
      this
    }
  }

  def +(eng: Double): Cell = Cell(x, y, value, energy + eng)

  def -(eng: Double): Cell = Cell(x, y, value, energy - eng)

  def ~(c: Color): Cell = Cell(x, y, c, energy)

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
}
