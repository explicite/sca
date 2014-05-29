package org.agh

import java.awt.Color

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
abstract class Boundaries {
  val width: Int
  val height: Int
  val permanent: Seq[Color]

  def mutate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Color] = {
    removePermanent {
      transform(coordinates)
    }
  }

  def transform(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)]

  def removePermanent(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Color] = {
    coordinates map toColor filter ifPermanent
  }

  private def ifPermanent(c: Color): Boolean = !permanent.contains(c)
  private def toColor(coordinate: (Int, Int))(implicit space: Seq[Cell]): Color = {
    coordinate match {
      case (x, y) => space(y + (height * x)).v
    }
  }
}

trait Periodic extends Boundaries {
  private def pX(x: Int): Int = ((x % width) + width) % width

  private def pY(y: Int): Int = ((y % height) + height) % height

  override def transform(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates map {
      case (x, y) => (pX(x), pY(y))
    }
  }
}

trait Absorbs extends Boundaries {
  override def transform(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates filter ifOutside
  }

  private def ifOutside(coordinate: (Int, Int)): Boolean = {
    coordinate match {
      case (x, y) => x >= 0 && y >= 0 && x < width && y < height
    }
  }
}