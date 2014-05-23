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

  def mutate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    removePermanent {
      transform(coordinates)
    }
  }

  def transform(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)]

  def removePermanent(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    coordinates map {
      case (x, y) => space(y + (height * x))
    } filter ifPermanent
  }

  private def ifPermanent(c: Cell): Boolean = !permanent.contains(c.v)
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

  private def ifOutside(xy: (Int, Int)): Boolean = {
    xy match {
      case (x, y) => x >= 0 && y >= 0 && x < width && y < height
    }
  }
}