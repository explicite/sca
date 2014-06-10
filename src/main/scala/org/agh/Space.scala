package org.agh

import scala.annotation._
import java.awt.Color._

// TODO functional approach eg. iterate(implicit seq)(n: seq=>seq)(b: seq=>seq): Seq
trait Space extends Neighbourhood {
  val permanent = BLACK :: WHITE :: Nil
  val probability = 0.7d

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

object Space extends Enumeration {
  val CA = Value("CA")
  val MC = Value("MC")
  val SPX= Value("SPX")
}

abstract case class CASpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in CA
   *
   * @param cels space to iterate
   * @return evaluated space
   */
  def iterate(implicit cels: Seq[Cell]): Seq[Cell] = {
    cels.map {
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

object CASpaceFactory {

  import Boundaries._
  import Neighbourhood._

  def apply(width: Int, height: Int, boundaries: Boundaries.Value, neighbourhood: Neighbourhood.Value): Space = {
    boundaries match {
      case Absorbs => neighbourhood match {
        case VonNeumann => new CASpace(width, height) with Absorbs with VonNeumann
        case NearestMoore => new CASpace(width, height) with Absorbs with NearestMoore
        case FurtherMoore => new CASpace(width, height) with Absorbs with FurtherMoore
        case RandomMoore => new CASpace(width, height) with Absorbs with RandomMoore
        case Moore => new CASpace(width, height) with Absorbs with Moore
        case Pentagonal => new CASpace(width, height) with Absorbs with Pentagonal
        case Hexagonal => new CASpace(width, height) with Absorbs with Hexagonal
      }
      case Periodic => neighbourhood match {
        case VonNeumann => new CASpace(width, height) with Periodic with VonNeumann
        case NearestMoore => new CASpace(width, height) with Periodic with NearestMoore
        case FurtherMoore => new CASpace(width, height) with Periodic with FurtherMoore
        case RandomMoore => new CASpace(width, height) with Periodic with RandomMoore
        case Moore => new CASpace(width, height) with Periodic with Moore
        case Pentagonal => new CASpace(width, height) with Periodic with Pentagonal
        case Hexagonal => new CASpace(width, height) with Periodic with Hexagonal
      }
    }
  }
}

object MCSpaceFactory {
  import Boundaries._
  import Neighbourhood._

  def apply(width: Int, height: Int, boundaries: Boundaries.Value, neighbourhood: Neighbourhood.Value): Space = {
    boundaries match {
      case Absorbs => neighbourhood match {
        case VonNeumann => new MCSpace(width, height) with Absorbs with VonNeumann
        case NearestMoore => new MCSpace(width, height) with Absorbs with NearestMoore
        case FurtherMoore => new MCSpace(width, height) with Absorbs with FurtherMoore
        case RandomMoore => new MCSpace(width, height) with Absorbs with RandomMoore
        case Moore => new MCSpace(width, height) with Absorbs with Moore
        case Pentagonal => new MCSpace(width, height) with Absorbs with Pentagonal
        case Hexagonal => new MCSpace(width, height) with Absorbs with Hexagonal
      }
      case Periodic => neighbourhood match {
        case VonNeumann => new MCSpace(width, height) with Periodic with VonNeumann
        case NearestMoore => new MCSpace(width, height) with Periodic with NearestMoore
        case FurtherMoore => new MCSpace(width, height) with Periodic with FurtherMoore
        case RandomMoore => new MCSpace(width, height) with Periodic with RandomMoore
        case Moore => new MCSpace(width, height) with Periodic with Moore
        case Pentagonal => new MCSpace(width, height) with Periodic with Pentagonal
        case Hexagonal => new MCSpace(width, height) with Periodic with Hexagonal
      }
    }
  }
}