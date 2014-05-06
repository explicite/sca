package org.agh

import java.awt.Color

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
abstract class Boundaries {
  val width: Int
  val height: Int
  val inactive: Seq[Color]

  def mutate(neighbours: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell]

  def removeInactive(ts: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    ts map(t => space(t._2 + (height * t._1))) filter(c => !inactive.contains(c.v))
  }
}

trait Periodic extends Boundaries {
  private def pX(x: Int): Int = ((x % width) + width) % width

  private def pY(y: Int): Int = ((y % height) + height) % height

  override def mutate(neighbours: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    removeInactive{
      neighbours map(p => (pX(p._1), pY(p._2)))
    }
  }
}

trait Absorbs extends Boundaries {
  override def mutate(neighbours: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    removeInactive{
      neighbours filter(p => p._1 >= 0 && p._2 >= 0 && p._1 < width && p._2 < height)
    }
  }
}