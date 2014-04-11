package org.agh

import java.awt.Color


/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract case class Space(width: Int, height: Int) extends Neighbours {

  def iterate(space: Seq[Cell]): Seq[Cell] = {
    implicit def coordinatesToCell(t: (Int, Int)): Cell = space(t._1 + width * t._2)

    def cellValue(x: Int, y: Int): Cell = {
      (x, y).v match {
        case Color.WHITE => {
          neighbours(x, y) match {
            case Nil => (x, y)
            case n: Seq[(Int, Int)] => {
              val values: Seq[(Int, Int)] = n filter (x => x.v != Color.WHITE && x.v != Color.BLACK)
              val valuesWithCount = values map (v => (v, values.count(_ == x)))
              val sorted = valuesWithCount.sortWith(_._2 > _._2)
              sorted match {
                case h :: tail => h._1
                case _ => (x, y)
              }
            }
          }
        }
        case _ => (x, y)
      }
    }

    space.map(c => cellValue(c.x, c.y))
  }

}
