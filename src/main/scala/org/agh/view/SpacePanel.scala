package org.agh.view

import java.awt.{Dimension, Color, Graphics}
import javax.swing.JComponent
import scala.swing.Component
import org.agh.Cell
import scala.util.Random

/**
 * @author Jan Paw
 *         date: 3/18/14
 */
class SpacePanel(width: Int, height: Int, cellSize: Int) extends Component {
  var space: Seq[Cell] = Nil

  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      space foreach (c => {
        g.setColor(c.v)
        g.fillRect(c.x * cellSize, c.y * cellSize, cellSize, cellSize)
      })
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
    peer.repaint()
  }

  def generate() = {
    space = Nil
    val rand = new Random()
    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val color: Color = rand.nextFloat() match {
          case x: Float if x > 0.9f => if (x > 0.99f) Color.BLACK else Color.getHSBColor(rand.nextFloat(), 1f, 1f)
          case _ => Color.WHITE
        }

        space ++= Cell(x, y, color) :: Nil
      }
    }
  }
}
