package org.agh

import scala.annotation._
import java.awt.Color._

// TODO functional approach eg. iterate(implicit seq)(n: seq=>seq)(b: seq=>seq): Seq
trait Space extends Neighbourhood {
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

object Space {
  import scala.reflect.runtime.universe.typeOf
  val CA = ("CASpace", typeOf[CASpace])
  val MC = ("MCSpace", typeOf[MCSpace])
  val SPX =("SRXSpace", typeOf[SRXSpace])
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
    val toEvaluate = edges
    val afterIterate = scala.collection.mutable.Seq(cells: _*)

    for (cell <- toEvaluate) {
      val neighbours = states(cell.x, cell.y)
      val energyBefore = cell.energy(neighbours)
      val newState = RANDOM.shuffle(neighbours).head
      val cellAfter = Cell(cell.x, cell.y, newState)
      val energyAfter = cellAfter.energy(neighbours)

      if (energyAfter < energyBefore) {
        afterIterate(cell.y + (height * cell.x)) = cellAfter
      }
    }

    afterIterate.toSeq
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
