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

  def mutate(xys: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    removePermanent {
      transform(xys)
    }
  }

  def transform(xys: Seq[(Int, Int)]): Seq[(Int, Int)]

  def removePermanent(ts: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Cell] = {
    ts map (t => space(t._2 + (height * t._1))) filter (c => !permanent.contains(c.v))
  }
}

trait Periodic extends Boundaries {
  private def pX(x: Int): Int = ((x % width) + width) % width

  private def pY(y: Int): Int = ((y % height) + height) % height

  override def transform(xys: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    xys map (p => (pX(p._1), pY(p._2)))
  }
}

trait Absorbs extends Boundaries {
  private def isOutside(xy: (Int, Int)): Boolean = {
    xy._1 >= 0 && xy._2 >= 0 && xy._1 < width && xy._2 < height
  }
  
  override def transform(xys: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      xys filter isOutside
  }
}