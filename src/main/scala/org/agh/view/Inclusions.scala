package org.agh.view

import org.agh._
import java.awt.Color._
import org.agh.Cell
import scala.annotation.switch

trait Inclusions {
  val width: Int
  val height: Int
  var cells: Seq[Cell]

  /**
   * Insert inclusions to space
   *
   * @param numberOfInclusions  on space
   * @param maxRadius  for inclusions
   * @return space with inclusions
   */
  def setInclusions(numberOfInclusions: Int, maxRadius: Int)(implicit space: Space) = {
    implicit val spaceWithInclusions = scala.collection.mutable.Seq(cells: _*)

    for (inc <- 0 until numberOfInclusions) {
      val draw = randomCell(cells)
      val radius = randomInt(maxRadius)

      space transforms inclusion(draw.x, draw.y, radius) foreach{
        case(x, y) =>
          spaceWithInclusions(y + (x * space.height)) = Cell(x, y, BLACK)
      }
    }

    cells = spaceWithInclusions
  }

  private def inclusion(x: Int, y: Int, size: Int): Seq[(Int, Int)] = {
    (randomBoolean: @switch) match {
      case true => circleInclusion(x, y, size)
      case false => rectInclusion(x, y, size)
    }
  }

  private def circleInclusion(x0: Int, y0: Int, radius: Int): Seq[(Int, Int)] = {
    var inclusion: Seq[(Int, Int)] = Seq.empty
    var x = radius
    var y = 0
    var xChange = 1 - (radius << 1)
    var yChange = 0
    var radiusError = 0

    while (x >= y) {
      var i = x0 - x
      while (i < x0 + x) {
        inclusion ++= (i, y0 + y) :: Nil
        inclusion ++= (i, y0 - y) :: Nil
        i += 1
      }

      i = x0 - y
      while (i < x0 + y) {
        inclusion ++= (i, y0 + x) :: Nil
        inclusion ++= (i, y0 - x) :: Nil
        i += 1
      }

      y += 1
      radiusError += yChange
      yChange += 2
      if (((radiusError << 1) + xChange) > 0) {
        x -= 1
        radiusError += xChange
        xChange += 2
      }
    }

    inclusion
  }

  private def rectInclusion(x0: Int, y0: Int, edge: Int): Seq[(Int, Int)] = {
    var inclusion: Seq[(Int, Int)] = Seq.empty
    var x = 0
    var y = 0

    while (x < edge) {
      while (y < edge) {
        inclusion ++= (x0 + x, y0 + y) :: Nil
        y += 1
      }
      y = 0
      x += 1
    }

    inclusion
  }
}
