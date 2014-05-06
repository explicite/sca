package org.agh.view

import java.awt.{Dimension, Graphics}
import javax.swing.JComponent
import scala.swing.Component
import org.agh.Cell
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
        i += 1;
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

  def generate(p: Float, n: Float): Unit = {
    val rand = new Random()
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      future {
        Cell(x, y, (rand.nextFloat(): @switch) match {
          case x: Float if x > p => if (x > n) BLACK else getHSBColor(rand.nextFloat(), 1f, 1f)
          case _ => WHITE
        })
      }
    }

    space = futures.map(c => Await.result(c, 100 milli))(breakOut)

  }
}
