package org.agh

import java.awt.Color
import java.awt.Color._

import scala.annotation.switch

abstract class Neighbourhood
  extends Boundaries
  with ShapeControl {

  protected val probability: Double = 0.7d

  def toCA(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    cell.value == WHITE
  }

  def toMC(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    onEdge(cell) &&  cell.value != BLACK
  }

  def toSRX(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    onEdge(cell) && !cell.recrystallized  && cell.value != BLACK
  }

  def onEdge(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    val (x, y) = (cell.x, cell.y)
    val values = (transforms _ andThen evaluate)(milieu(x, y))
    val uniqueValues = values groupBy (_.getRGB)

    (uniqueValues.size: @switch) match {
      case 1 => false
      case _ => values.size != uniqueValues.size
    }
  }

  def edges(implicit space: Seq[Cell]): Seq[Cell] = {
    space filter (cell => onEdge(cell))
  }

  def states(cell: Cell)(implicit space: Seq[Cell]): Seq[Color] = {
    mutate(coordinates(cell)) map toColor
  }

  def value(cell: Cell)(implicit space: Seq[Cell]): Option[Color] = {
    (mutate _ andThen mapToColor andThen expand)(coordinates(cell))
  }

  protected def coordinates(cell: Cell): Seq[(Int, Int)]

  private def milieu(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y) ::(x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }

  def neighbours(cell: Cell)(implicit space: Seq[Cell]): Seq[Cell] = {
    mutate(coordinates(cell))
  }
}

object Neighbourhood extends Enumeration {

  import scala.reflect.runtime.universe.typeOf

  val VonNeumann = ("Von Neumann", typeOf[VonNeumann])
  val ExtendedMoore = ("Extended Moore", typeOf[ExtendedMoore])
  val Moore = ("Moore", typeOf[Moore])
  val Pentagonal = ("Pentagonal", typeOf[Pentagonal])
  val Hexagonal = ("Hexagonal", typeOf[Hexagonal])
}

trait VonNeumann extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (x, y - 1) ::(x, y + 1) ::(x - 1, y) ::(x + 1, y) :: Nil
  }
}

trait NearestMoore extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
  }
}

trait FurtherMoore extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
  }
}

trait ExtendedMoore extends Neighbourhood with Convexity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

trait Moore extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

trait Pentagonal extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (randomCase(4): @switch) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) :: Nil
      case 2 => (x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
      case 3 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
      case 4 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
    }
  }
}

trait Hexagonal extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (randomCase(2): @switch) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
      case 2 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) :: Nil
    }
  }
}

trait ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color]
}

trait Concavity extends ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color] = {
    (cells: @switch) match {
      case head :: tail =>
        val cellsWithCount = cells map (c => (c, cells.count(_ == c)))
        cellsWithCount.sortWith(_._2 > _._2).headOption map (_._1)
      case Nil => None
    }
  }
}

trait Convexity extends ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color] = {
    //TODO
    (cells: @switch) match {
      case head :: tail =>
        val cellsWithCount = cells map (c => (c, cells.count(_ == c)))
        cellsWithCount.maxBy(_._2)._2 match {
          case 5 | 6 | 7 | 8 => cellsWithCount.headOption map (_._1)
          case 3 | 4 =>
            (cells(0) :: cells(2) :: cells(5) :: cells(7) :: Nil).map {
              c =>
                (c, cells.count(_ == c))
            }.maxBy(_._2) match {
              case (color, 4) => Some(color)
              case (color, 3) => Some(color)
              case (color, 3) =>
                (cells(1) :: cells(3) :: cells(4) :: cells(6) :: Nil).map {
                  c => (c, cells.count(_ == c))
                }.maxBy(_._2) match {
                  case (color, 4) => Some(color)
                  case (color, 3) => Some(color)
                  case _ => (randomCase(2): @switch) match {
                    case 1 => cellsWithCount.headOption map (_._1)
                    case 2 => None
                  }
                }
            }
          case _ => (randomCase(2): @switch) match {
            case 1 => cellsWithCount.headOption map (_._1)
            case 2 => None
          }
        }
      case Nil => None
    }
  }
}