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
  val width = 800
  val height = 600
  val cellSize = 1
  val space = new Space(width, height) with RandomMoore with Periodic

  lazy val canvas = new SpacePanel(width, height, cellSize)
  lazy val iterate = new Button("iterate")

  lazy val panel = new FlowPanel() {
    contents ++= canvas :: iterate :: Nil
  }

  canvas generate(0.9987654321f, 0.9999f)

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
