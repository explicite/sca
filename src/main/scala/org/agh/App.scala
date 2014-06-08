package org.agh

import java.awt.event.MouseEvent.{BUTTON1 => LeftButton}
import scala.swing.event.{MouseClicked, ButtonClicked}
import scala.swing.event.Key.Modifier.Control
import scala.swing.event.Key.Modifier.Shift
import org.agh.view.SpacePanel
import javax.swing.UIManager
import scala.swing._

object App extends SwingApplication {
  val width = 500
  val height = 500
  val cellSize = 1
  implicit val space = new CASpace(width, height) with RandomMoore with Periodic

  lazy val canvas = new SpacePanel(width, height, cellSize)
  lazy val iterate: Button = "iterate"
  lazy val edges: Button = "remove edges"
  lazy val active: Button = "remove active"
  lazy val inactive: Button = "remove inactive"
  lazy val circleInclusionsButton: Button = "apply"
  lazy val squareInclusionsButton: Button = "apply"

  lazy val circleInclusionsField: TextField = 0
  lazy val squareInclusionsField: TextField = 0

  lazy val circleInclusionsLabel: Label = "circle"
  lazy val squareInclusionsLabel: Label = "square"

  lazy val inclusionsMenu = new GridPanel(2, 3) {
    contents ++= circleInclusionsField :: circleInclusionsLabel :: circleInclusionsButton ::
      squareInclusionsField :: squareInclusionsLabel :: squareInclusionsButton :: Nil
    border = "inclusions"
  }

  lazy val buttons = new GridPanel(2, 2) {
    contents ++= iterate :: edges :: active :: inactive :: Nil
    border = "activity"
  }

  lazy val menu = new GridPanel(2, 1) {
    contents ++= inclusionsMenu :: buttons :: Nil
    border = "menu"
  }

  lazy val content = new FlowPanel() {
    contents ++= canvas :: menu :: Nil
  }

  def top = new MainFrame {
    title = "SCA"
    contents = content

    listenTo(
      iterate,
      edges,
      active,
      inactive,
      circleInclusionsButton,
      squareInclusionsButton,
      canvas.mouse.clicks
    )

    reactions += {
      case ButtonClicked(`iterate`) =>
        canvas.iterate
      case ButtonClicked(`edges`) =>
        canvas.onTheEdge((c: Cell) => Cell(c.x, c.y))
      case ButtonClicked(`active`) =>
        canvas.removeActive()
      case ButtonClicked(`inactive`) =>
        canvas.removeInactive()
      case ButtonClicked(`circleInclusionsButton`) =>
        canvas.setCircleInclusions(circleInclusionsField)
      case ButtonClicked(`squareInclusionsButton`) =>
        canvas.setSquareInclusions(squareInclusionsField)
      case e: MouseClicked => e.peer.getButton match {
        case LeftButton => e.modifiers match {
          case Control => canvas.selectGrain(e)
          case Shift => canvas.insertRandom(e)
          case _ =>
        }
        case _ =>
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
