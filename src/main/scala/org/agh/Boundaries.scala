package org.agh

import java.awt.Color

abstract class Boundaries {
  val width: Int
  val height: Int

  def mutate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    (transforms _ andThen evaluate)(coordinates)
  }

  def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)]

  def evaluate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    coordinates map toCell filter permanent
  }

  def toCell(coordinate: (Int, Int))(implicit space: Seq[Cell]): Cell = {
    coordinate match {
      case (x, y) => space(y + (height * x))
    }
  }

  def toColor(c: Cell): Color = c.value

  def mapToColor(cells: Seq[Cell]): Seq[Color]= cells map toColor

  def permanent(cell: Cell): Boolean = {
    !cell.permanent
  }
}

object Boundaries {

  import scala.reflect.runtime.universe.typeOf

  val Absorbs = ("Absorbs", typeOf[Absorbs])
  val Periodic = ("Periodic", typeOf[Periodic])


}

trait Periodic extends Boundaries {
  override def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates map transform
  }

  private def transform(coordinate: (Int, Int)): (Int, Int) = {
    coordinate match {
      case (x, y) => (((x % width) + width) % width, ((y % height) + height) % height)
    }
  }
}

trait Absorbs extends Boundaries {
  override def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates filter predicate
  }

  private def predicate(coordinate: (Int, Int)): Boolean = {
    coordinate match {
      case (x, y) => x >= 0 && y >= 0 && x < width && y < height
    }
  }
}