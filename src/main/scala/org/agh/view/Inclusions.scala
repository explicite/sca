package org.agh.view

import scala.annotation.switch
import java.awt.Color._
import org.agh._
import scala.collection.mutable

trait Inclusions {
  val width: Int
  val height: Int
  implicit var cells: Seq[Cell]

  def repaint(): Unit

  /**
   * Insert inclusions to space
   *
   * @param numberOfInclusions  on space
   * @param maxRadius  for inclusions
   * @return space with inclusions
   */
  def setRandomInclusions(numberOfInclusions: Int, maxRadius: Int)(implicit space: Space) = {
    implicit val spaceWithInclusions = scala.collection.mutable.Seq(cells: _*)

    for (inclusion <- 0 until numberOfInclusions) {
      val draw = randomCell
      val radius = randomInt(maxRadius)

      space transforms randomInclusion(draw.x, draw.y, radius) foreach {
        case (x, y) =>
          spaceWithInclusions(y + (x * space.height)) = Cell(x, y, BLACK)
      }
    }

    cells = spaceWithInclusions
    repaint()
  }

  def setCircleInclusions(numberOfInclusions: Int)(implicit space: Space): Unit = {
    setInclusions(numberOfInclusions)(circleInclusion)
  }

  def setRectInclusions(numberOfInclusions: Int)(implicit space: Space): Unit = {
    setInclusions(numberOfInclusions)(rectInclusion)
  }


  private def setInclusions(numberOfInclusions: Int)(insert: (Int, Int, Int) => Seq[(Int, Int)])(implicit space: Space): Unit ={
    implicit val spaceWithInclusions = scala.collection.mutable.Seq(cells: _*)
    val maxRadius = ((if (width > height) height else width) * 0.01).toInt

    for (inclusion <- 0 until numberOfInclusions) {
      val draw = randomCell
      val radius = randomInt(maxRadius)

      space transforms insert(draw.x, draw.y, radius) foreach {
        case (x,y) =>
          spaceWithInclusions(y + (x * space.height)) = Cell(x, y, BLACK)
      }
    }

    cells = spaceWithInclusions.toSeq
    repaint()
  }

  private def randomInclusion(x: Int, y: Int, size: Int): Seq[(Int, Int)] = {
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
