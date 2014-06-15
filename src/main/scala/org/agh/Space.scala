package org.agh

import java.awt.Color
import java.awt.Color._

import scala.annotation._
import scala.collection._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

// TODO functional approach eg. iterate(implicit seq)(n: seq=>seq)(b: seq=>seq): Seq
trait Space extends Neighbourhood with Energy {
  /**
   * Iteration over all cells in space
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  def iterate(implicit cells: Seq[Cell]): Seq[Cell]

  def modify(modifier: Cell => Cell)(implicit space: Seq[Cell]): Seq[Cell] = {
    space map modifier
  }

  def onTheEdge(modify: Cell => Cell)(implicit space: Seq[Cell]): Seq[Cell] = {
    space.map {
      case cell => isEdge(cell.x, cell.y) match {
        case true => modify(cell)
        case false => cell
      }
    }
  }
}

trait Energy {
  def ei(states: Seq[Color], oldState: Color, newState: Color): Double = {
    val afterEnergy = states.map { state => en(states, newState)}
    val beforeEnergy = states.map { state => en(states, oldState)}

    (afterEnergy zip beforeEnergy).map {
      case (after, before) =>
        math.exp(-(after - before) / 0.6)
    }.sum
  }

  def en(s: Seq[Color], c: Color): Double = {
    s.map {
      case `c` => 0.0
      case _ => 1.0
    }.sum
  }

  def value(Δe: Double): Double = {
    if (Δe > 0)
      math.exp(-Δe / 0.6)
    else
      1
  }
}

object Space {

  import scala.reflect.runtime.universe.typeOf

  val CA = ("CASpace", typeOf[CASpace])
  val MC = ("MCSpace", typeOf[MCSpace])
  val SPX = ("SRXSpace", typeOf[SRXSpace])
}

abstract case class CASpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in CA
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  def iterate(implicit cells: Seq[Cell]): Seq[Cell] = {
    cells.map {
      c => (c.value: @switch) match {
        case WHITE => c(value)
        case _ => c
      }
    }
  }
}

abstract case class MCSpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in Monte Carlo. Consider only cells on grains boundary.
   * Selection of a new orientation only from neighbors. Cell is randomly generated from (W-k)
   * available cells, where k is a number of cells  already considerated in currently MC step.
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  override def iterate(implicit cells: Seq[Cell]): Seq[Cell] = {
    val futures = cells.map {
      cell =>
        Future {
          if (isEdge(cell)) {
            val ss = states(cell.x, cell.y)
            val beforeState = cell.value
            val afterState = RANDOM.shuffle(ss).head
            val beforeEnergy = en(ss, beforeState)
            val afterEnergy = en(ss, afterState)

            if (afterEnergy - beforeEnergy <= 0) {
              Cell(cell.x, cell.y, afterState)
            } else {
              cell
            }
          } else {
            cell
          }
        }
    }

    futures.map(c => Await.result(c, 500 milli))(breakOut)
  }
}

abstract case class SRXSpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in SRX
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  override def iterate(implicit cells: Seq[Cell]): Seq[Cell] = ???
}

object SpaceFactory {

  import scala.reflect.runtime.universe._
  import scala.tools.reflect._

  val toolbox = reflect.runtime.currentMirror.mkToolBox()

  /**
   * Based on reflection [[org.agh.Space]] factory
   *
   * @param width space width
   * @param height space height
   * @param types ([[org.agh.Space]], [[org.agh.Neighbourhood]], [[org.agh.Boundaries]]])
   * @return
   */
  def apply(width: Int, height: Int)(types: (Type, Type, Type)): Space = {
    val (space, neighbours, boundaries) = types
    val tree = q"new $space($width, $height) with $neighbours with $boundaries"

    toolbox.eval(tree).asInstanceOf[Space]
  }
}
