package org.agh

import javax.swing.UIManager
import scala.swing._
import org.agh.view.SpacePanel
import scala.swing.event.ButtonClicked
import java.awt.Color

/**
 * @author Jan Paw 
 *         Date: 3/18/14
 */
object App extends SwingApplication {
  val width = 100
  val height = 100
  val cellSize = 2
  val space = new Space(width, height) with Moore with Periodic

  lazy val canvas = new SpacePanel(width, height, cellSize)
  lazy val iterate = new Button("iterate")

  lazy val panel = new FlowPanel() {
    contents ++= canvas :: iterate :: Nil
  }

  canvas.paint({
    val cells = scala.collection.mutable.Seq.fill(width * height)(0f)
    cells((height / 5)*height + (width / 2)) = 0.1f
    cells.seq
  })

  def top = new MainFrame {
    title = "SCA"
    contents = panel

    listenTo(iterate)
    reactions += {
      case ButtonClicked(`iterate`) =>
        canvas.paint(space.iterate(canvas.space))
    }

  }

  override def startup(args: Array[String]) {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
