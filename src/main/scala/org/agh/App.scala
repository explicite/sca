package org.agh

import java.awt.event.MouseEvent.{BUTTON1 => LeftButton}
import scala.swing.event.{MouseClicked, ButtonClicked}
import scala.swing.event.Key.Modifier.Control
import scala.swing.event.Key.Modifier.Shift
import scala.swing.Orientation._
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
  lazy val rectInclusionsButton: Button = "apply"

  lazy val nucleationButton: Button = "apply"

  lazy val circleInclusionsField: TextField = 0
  lazy val rectInclusionsField: TextField = 0

  lazy val circleInclusionsLabel: Label = "circle"
  lazy val rectInclusionsLabel: Label = "rect"

  lazy val nucleationField: TextField = 0
  lazy val nucleationLabel: Label = "seeds"

  lazy val inclusionsMenu = new GridPanel(2, 3) {
    contents ++= circleInclusionsField :: circleInclusionsLabel :: circleInclusionsButton ::
      rectInclusionsField :: rectInclusionsLabel :: rectInclusionsButton :: Nil
    border = "inclusions"
  }

  lazy val nucleationMenu = new GridPanel(1, 3) {
    contents ++= nucleationField :: nucleationLabel :: nucleationButton :: Nil
    border = "nucleation"
  }

  lazy val activity = new GridPanel(2, 2) {
    contents ++= iterate :: edges :: active :: inactive :: Nil
    border = "activity"
  }

  lazy val menu = new BoxPanel(Vertical){
    contents ++= inclusionsMenu :: nucleationMenu :: activity :: Nil
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
      rectInclusionsButton,
      nucleationButton,
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
      case ButtonClicked(`rectInclusionsButton`) =>
        canvas.setRectInclusions(rectInclusionsField)
      case ButtonClicked(`nucleationButton`) =>
        canvas.setNucleation(nucleationField)
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
