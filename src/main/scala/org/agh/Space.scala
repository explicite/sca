package org.agh

import java.awt.Color._

import scala.annotation._
import scala.collection._

// TODO functional approach eg. iterate(implicit seq)(n: seq=>seq)(b: seq=>seq): Seq
trait Space extends Neighbourhood with Nucleation with Distribution {
  implicit val space = this

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
      case cell => edge(cell.x, cell.y) match {
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
    cells.par.map {
      cell => (cell.value: @switch) match {
        case BLACK => cell
        case _ => edge(cell) match {
          case true => cell.applyMC(states(cell))
          case _ => cell
        }
      }
    }.seq
  }
}

abstract case class SRXSpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in SRX
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  override def iterate(implicit cells: Seq[Cell]): Seq[Cell] = {
    nucleation.par.map {
      cell => (cell.value: @switch) match {
        case BLACK => cell
        case _ =>
          if(edge(cell) && !cell.recrystallized)
            cell.applySRX(neighbours(cell) filter (_.recrystallized))
          else cell
      }
    }.seq
  }
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
   * @param types ([[org.agh.Space]], [[org.agh.Neighbourhood]], [[org.agh.Boundaries]], [[org.agh.Nucleation]], [[org.agh.Distribution]])
   * @return
   */
  def apply(width: Int, height: Int)(types: (Type, Type, Type, Type, Type)): Space = {
    val (space, neighbours, boundaries, nucleation, distribution) = types
    val tree = q"new $space($width, $height) with $neighbours with $boundaries with $nucleation with $distribution"

    toolbox.eval(tree).asInstanceOf[Space]
  }
}
