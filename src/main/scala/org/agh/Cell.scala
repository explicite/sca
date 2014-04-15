package org.agh

import java.awt.Color


/**
 * @author Jan Paw 
 *         Date: 3/17/14
 */
case class Cell(x: Int, y: Int, v: Color) {
  def apply(x: Int, y: Int, v: Color): Cell = new Cell(x, y, v)

  def evaluate(f: (Int, Int) => Option[Color]): Cell = {
    val value = f(x, y)

    value match {
      case Some(v) => Cell(x, y, v)
      case None => this
    }
  }
}
