package org.agh.view

import java.awt.{Dimension, Graphics}
import javax.swing.JComponent
import scala.swing.Component
import org.agh.{Space, Cell}
import java.awt.Color._
import scala.util.Random
import scala.annotation.switch
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.breakOut

/**
 * @author Jan Paw
 *         date: 3/18/14
 */
class SpacePanel(width: Int, height: Int, cellSize: Int) extends Component {
  var space: Seq[Cell] = Nil

  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      var i = 0
      while (i < space.length) {
        val c = space(i)
        g.setColor(c.v)
        g.fillRect(c.x * cellSize, c.y * cellSize, cellSize, cellSize)
        i += 1
      }
    }

    override def update(g: Graphics) = {
      val offScreen = createImage(width * cellSize, height * cellSize)
      val offGraphics = offScreen.getGraphics

      paint(offGraphics)
      g.drawImage(offScreen, 0, 0, this)
    }
  }

  def paint(s: Seq[Cell]) = {
    space = s
    repaint()
  }

  /**
   * Generate space
   *
   * @param seeds numebr of nucleation cells
   * @param inclusions number of inactive cells
   */
  def generate(seeds: Float = 1f, inclusions: Float = 1f): Unit = {
    val rand = new Random()
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      future {
        Cell(x, y, (rand.nextFloat(): @switch) match {
          case x: Float if x > seeds => if (x > inclusions) BLACK else getHSBColor(rand.nextFloat(), 1f, 1f)
          case _ => WHITE
        })
      }
    }

    space = futures.map(c => Await.result(c, 100 milli))(breakOut)
  }


  /**
   * Insert inclusions to space
   *
   * @param numberOfInclusions  on space
   * @param maxRadius  for inclusions
   * @return space with inclusions
   */
  def setInclusions(numberOfInclusions: Int, maxRadius: Int)(implicit s: Space) = {
    implicit val spaceWithInclusions = scala.collection.mutable.Seq(space: _*)

    for (inc <- 0 until numberOfInclusions) {
      val draw = Random.shuffle(space).head
      val radius = Random.nextInt(maxRadius)
      val cells = s transform inclusion(draw.x, draw.y, radius)
      for (cell <- cells)
        spaceWithInclusions(cell._2 + (cell._1 * s.height)) = Cell(cell._1, cell._2, BLACK)
    }

    space = spaceWithInclusions
  }

  private def inclusion(x: Int, y: Int, size: Int): Seq[(Int, Int)] = {
    (new Random().nextBoolean(): @switch) match {
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
