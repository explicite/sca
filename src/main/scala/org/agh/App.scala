package org.agh

import scala.swing._
import java.awt.event.MouseEvent.{BUTTON1 => LeftButton}
import javax.swing.UIManager
import org.agh.view.SpacePanel
import scala.swing.event.Key.Modifier.Control
import scala.swing.event.{MouseClicked, ButtonClicked}

/**
 * @author Jan Paw 
 *         Date: 3/18/14
 */
object App extends SwingApplication {
  val width = 500
  val height = 400
  val cellSize = 1
  implicit val space = new CASpace(width, height) with RandomMoore with Periodic

  lazy val canvas = new SpacePanel(width, height, cellSize)
  lazy val iterate = new Button("iterate")

  lazy val panel = new FlowPanel() {
    contents ++= canvas :: iterate :: Nil
  }

  canvas generate 0.9987654321f
  canvas setInclusions(20, 5)

  def top = new MainFrame {
    title = "SCA"
    contents = panel
    val point: Point = new Point

    listenTo(iterate)
    listenTo(canvas.mouse.clicks)
    reactions += {
      case ButtonClicked(`iterate`) =>
        canvas.paint(space.iterate(canvas.space))
      case e: MouseClicked => e.peer.getButton match {
        case LeftButton => e.modifiers match {
          case Control => println(s"left clicked at $e")
          case _ => Some
        }
        case _ => Some
      }
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
