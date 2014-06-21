package org.agh

import scala.collection._

// TODO functional approach eg. iterate(implicit seq)(n: seq=>seq)(b: seq=>seq): Seq
trait Space extends Neighbourhood with Nucleation with Distribution {
  implicit val space = this

  def apply(cell: Cell)(implicit cells: Seq[Cell]): Cell

  def allow(cell: Cell)(implicit space: Seq[Cell]): Boolean

  /**
   * Iteration over all cells in space
   *
   * @param cells space to iterate
   * @return evaluated space
   */
  def iterate(implicit cells: Seq[Cell], context: NucleationContext = NucleationContext.One): Seq[Cell] = {
    nucleation.par.map{
      cell =>
        if(allow(cell)) apply(cell) else cell
    }.seq
  }

  def modify(modifier: Cell => Cell)(implicit space: Seq[Cell]): Seq[Cell] = {
    space map modifier
  }

  def onTheEdge(modify: Cell => Cell)(implicit space: Seq[Cell]): Seq[Cell] = {
    space.map {
      case cell => onEdge(cell) match {
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
  def allow(cell: Cell)(implicit space: Seq[Cell]): Boolean = allowCa(cell)

  def apply(cell: Cell)(implicit cells: Seq[Cell]): Cell = {
    cell(value)
  }
}

abstract case class MCSpace(width: Int, height: Int) extends Space {
  def allow(cell: Cell)(implicit space: Seq[Cell]): Boolean = allowMC(cell)

  def apply(cell: Cell)(implicit cells: Seq[Cell]): Cell = {
    val states = neighbours(cell).map(_.value)
    if (states.nonEmpty) {
      val beforeState = cell.value
      val afterState = RANDOM.shuffle(states).head
      val beforeEnergy = cell.energy(states, beforeState)
      val afterEnergy = cell.energy(states, afterState)

      if (afterEnergy - beforeEnergy <= 0) cell ~ afterState else cell

    } else cell
  }
}

abstract case class SRXSpace(width: Int, height: Int) extends Space {
  def allow(cell: Cell)(implicit space: Seq[Cell]): Boolean = allowSRX(cell)

  def apply(cell: Cell)(implicit cells: Seq[Cell]): Cell = {
    val states = neighbours(cell).filter(_.recrystallized).map(_.value)
    if (states.nonEmpty) {
      val afterState = RANDOM.shuffle(states).head
      val beforeEnergy = (cell.energy(states, cell) * 0.5) + cell.energy
      val afterEnergy = cell.energy(states, afterState) * 0.5

      if (afterEnergy <= beforeEnergy) cell ~ afterState ~ true ~ 0 else cell

    } else cell
  }

  override def iterate(implicit cells: Seq[Cell], context: NucleationContext = NucleationContext.One): Seq[Cell] = {
    val cellsWithNucleation = nucleation
    cellsWithNucleation.par.map {
      cell => if(allow(cell)) apply(cell)(cellsWithNucleation) else cell
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
