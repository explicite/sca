package org.agh
package view

import scala.concurrent.ExecutionContext.Implicits.global
import java.awt.{Dimension, Graphics}
import scala.concurrent.duration._
import scala.collection.breakOut
import scala.annotation.switch
import javax.swing.JComponent
import scala.swing.Component
import scala.concurrent._
import java.awt.Color._
import org.agh.Cell

class SpacePanel(val width: Int, val height: Int, cellSize: Int)
  extends Component
  with Inclusions {

  var cells: Seq[Cell] = Nil

  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      var i = 0
      while (i < cells.length) {
        val c = cells(i)
        g.setColor(c)
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

  def paint(space: Seq[Cell]) = {
    cells = space
    repaint()
  }

  /**
   * Generate space
   *
   * @param seeds numebr of nucleation cells
   * @param inclusions number of inactive cells
   */
  def generate(seeds: Float = 1f, inclusions: Float = 1f): Unit = {
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      future {
        Cell(x, y, (randomFloat: @switch) match {
          case x: Float if x > seeds => if (x > inclusions) BLACK else getHSBColor(randomFloat, 1f, 1f)
          case _ => WHITE
        })
      }
    }

    cells = futures.map(c => Await.result(c, 100 milli))(breakOut)
  }
}
