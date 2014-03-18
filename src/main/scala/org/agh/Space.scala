package org.agh

/**
 * @author Jan 
 *         Date: 3/16/14
 */

abstract class Space extends Neighbours {

  def iterate(space: Seq[Double]): Seq[Double] = {
    var tmp: Seq[Double] = Nil
    def cellValue(x: Int, y: Int): Double = {
      val values = neighbours(x, y).map(c => space(c._1 + c._2 * height)).filter(x => x > 0)
      val valuesWithCount = values map (v => (v, values.count(_ == x)))
      val sorted = valuesWithCount.sortWith(_._2 > _._2)
      sorted match {
        case h :: tail => h._1
        case _ => 0
      }
    }

    for (x <- 0 until width)
      for (y <- 0 until height)
        tmp ++= cellValue(x, y) :: Nil

    tmp
  }
}

abstract case class MooreSpace(width: Int, height: Int) extends Space with Moore

abstract case class VonNeumannSpace(width: Int, height: Int) extends Space with VonNeumann

abstract case class HexagonalSpace(width: Int, height: Int) extends Space with Hexagonal

abstract case class PentagonalSpace(width: Int, height: Int) extends Space with Pentagonal
