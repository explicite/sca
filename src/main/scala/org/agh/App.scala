package org.agh

import java.awt.event.MouseEvent.{BUTTON1 => LeftButton}
import scala.swing.event.{MouseClicked, ButtonClicked}
import scala.swing.event.Key.Modifier.Control
import org.agh.view.SpacePanel
import javax.swing.{BorderFactory, UIManager}
import java.awt.Color
import scala.swing._

object App extends SwingApplication {
  val width = 500
  val height = 500
  val cellSize = 1
  implicit val space = new CASpace(width, height) with RandomMoore with Periodic

  lazy val canvas = new SpacePanel(width, height, cellSize)
  lazy val iterate = new Button("iterate")
  lazy val edges = new Button("remove edges")

  lazy val menu = new GridPanel(2, 1) {
    contents ++= iterate :: edges :: Nil
    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("menu"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val content = new FlowPanel() {
    contents ++= canvas :: menu :: Nil
  }

  canvas generate 0.9987654321f
  canvas setInclusions(30, 10)

  def top = new MainFrame {
    title = "SCA"
    contents = content
    val point: Point = new Point

    listenTo(iterate, edges)
    listenTo(canvas.mouse.clicks)
    reactions += {
      case ButtonClicked(`iterate`) =>
        canvas.iterate
      case ButtonClicked(`edges`) =>
        canvas.onTheEdge((c: Cell) => Cell(c.x, c.y, Color.WHITE))
      case e: MouseClicked => e.peer.getButton match {
        case LeftButton => e.modifiers match {
          case Control => canvas.selectGrain(e)
          case _ => Unit
        }
        case _ => Unit
      }
    }
  }

  override def startup(args: Array[String]) {
    UIManager.setLookAndFeel {
      UIManager.getSystemLookAndFeelClassName
    }
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
