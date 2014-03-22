package org.agh.view

import java.awt.{Dimension, Color, Graphics}
import javax.swing.JComponent
import scala.swing.Component


/**
 * @author Jan Paw
 *         date: 3/18/14
 */
class SpacePanel(width: Int, height: Int, cellSize: Int) extends Component {
  var space: Seq[Float] = Nil
  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val c = Color.getHSBColor(space(y + x * width), 1f, 1f)
          g.setColor(c)
          g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
        }
      }
    }

    override def update(g: Graphics) = {
      val offScreen = createImage(width * cellSize, height * cellSize)
      val offGraphics = offScreen.getGraphics

      paint(offGraphics)
      g.drawImage(offScreen, 0, 0, this)
    }
  }

  def paint(s: Seq[Float]) = {
    space = s
    peer.repaint()
  }
}
