package org.agh

import java.awt.Color
import java.awt.Color._

import scala.annotation.switch

abstract class Neighbourhood
  extends Boundaries
  with ShapeControl {

  protected val probability: Double = 0.7d

  def allowCa(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    cell.value == WHITE
  }

  def allowMC(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    onEdge(cell) && cell.value != BLACK
  }

  def allowSRX(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    cell.value != BLACK
  }

  def onEdge(cell: Cell)(implicit space: Seq[Cell]): Boolean = {
    val (x, y) = (cell.x, cell.y)
    val values = (transforms _ andThen evaluate)(milieu(x, y)).filterNot(_.recrystallized)
    val uniqueValues = values groupBy (_.getRGB)

    (uniqueValues.size: @switch) match {
      case 1 => false
      case _ => values.size != uniqueValues.size
    }
  }

  def edges(implicit space: Seq[Cell]): Seq[Cell] = {
    space filter (cell => onEdge(cell))
  }

  def value(cell: Cell)(implicit space: Seq[Cell]): Option[Color] = {
    (mutate _ andThen expand)(coordinates(cell))
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
  val ExtendedMoore = ("Extended Moore", typeOf[RandomMoor])
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

trait RandomMoor extends Neighbourhood with Concavity {
  override protected def coordinates(cell: Cell): Seq[(Int, Int)] = {
    val (x, y) = (cell.x, cell.y)
    (randomCase(8): @switch) match {
      case 8 => (randomCase(2): @switch) match {
        case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
        case 2 => Nil
      }
      case 7| 6 | 5 | 4 | 3 => (randomCase(2): @switch) match {
        case 1 => (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
        case 2 => (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
      }
      case _ => (randomCase(2): @switch) match {
        case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
        case 2 => Nil
      }
    }
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
  protected def expand(cells: Seq[Cell]): Option[Color]
}

trait Concavity extends ShapeControl {
  protected def expand(cells: Seq[Cell]): Option[Color] = {
    val colors = cells.map(_.value)
    (colors: @switch) match {
      case head :: tail =>
        val colorsWithCount = colors map (c => (c, colors.count(_ == c)))
        colorsWithCount.sortWith(_._2 > _._2).headOption map (_._1)
      case Nil => None
    }
  }
}

trait Convexity extends ShapeControl {
  def nearest(cell: Cell, cells: Seq[Cell]): Boolean = {
    val (x, y) = (cell.x, cell.y)
    val coordinates = (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
    var n = 0
    cells.map(cell => (x, y)).foreach {
      crd => coordinates.contains(crd) match {
        case true => n + 1
        case false => Unit
      }
    }

    if (n >= 3) true else false
  }

  def further(cell: Cell, cells: Seq[Cell]): Boolean = {
    val (x, y) = (cell.x, cell.y)
    val coordinates = (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
    var n = 0
    cells.map(cell => (x, y)).foreach {
      crd => coordinates.contains(crd) match {
        case true => n + 1
        case false => Unit
      }
    }

    if (n >= 3) true else false
  }

  protected def expand(cells: Seq[Cell]): Option[Color] = {
    (cells: @switch) match {
      case head :: tail =>
        val colors = cells.map(_.value)
        val colorsWithCount = colors map (c => (c, colors.count(_ == c)))
        colorsWithCount.maxBy(_._2)._2 match {
          case 6 | 7 | 8 | 9 => colorsWithCount.headOption map (_._1)
          case 4 | 5 => nearest(cells.head, cells) match {
            case true => colorsWithCount.headOption map (_._1)
            case false => further(cells.head, cells) match {
              case true => colorsWithCount.headOption map (_._1)
              case false => (randomCase(2): @switch) match {
                case 1 => colorsWithCount.headOption map (_._1)
                case 2 => None
              }
            }
          }
          case _ => (randomCase(2): @switch) match {
            case 1 => colorsWithCount.headOption map (_._1)
            case 2 => None
          }
        }
      case Nil => None
    }
  }
}