package org.agh

import java.awt.Color


/**
 * @author Jan Paw 
 *         Date: 3/17/14
 */
case class Cell(x: Int, y: Int, v: Color) {
  def apply(x: Int, y: Int, v: Color): Cell = new Cell(x, y, v)

  def evaluate(n: Seq[Color]): Cell = {
    val colors = n.filter(c => c != Color.BLACK && c != Color.WHITE)
    val colorsWithCount = colors.map(v => (v, colors.count(_ == x)))
    val color = colorsWithCount.sortWith(_._2 > _._2) match {
      case head :: tail => head._1
      case _ => v
    }

    Cell(x, y, color)
  }
}

object Cell {
  implicit def cellToColor(c: Cell): Color = c.v
}
