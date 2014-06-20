package org.agh

import java.awt.event.MouseEvent.{BUTTON1 => LeftButton}
import javax.swing.UIManager

import org.agh.Boundaries._
import org.agh.Distribution._
import org.agh.Neighbourhood._
import org.agh.Nucleation._
import org.agh.Space._
import org.agh.view.SpacePanel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.runtime.universe.Type
import scala.swing.Orientation._
import scala.swing._
import scala.swing.event.Key.Modifier.{Control, Shift}
import scala.swing.event.{ButtonClicked, MouseClicked, SelectionChanged}


object App extends SwingApplication {
  val width = 500
  val height = 500
  val cellSize = 1
  implicit var space: Space = SpaceFactory(width, height)(CA, VonNeumann, Absorbs, Lack, Heterogenous)

  lazy val canvas = new SpacePanel(width, height, cellSize)

  lazy val iterate: Button = "iterate"

  lazy val edges: Button = "remove edges"

  lazy val active: Button = "remove active"
  lazy val inactive: Button = "remove inactive"

  lazy val nucleationButton: Button = "apply"

  lazy val circleInclusionsButton: Button = "apply"
  lazy val circleInclusionsField: TextField = 0
  lazy val circleInclusionsLabel: Label = "circle"

  lazy val rectInclusionsButton: Button = "apply"
  lazy val rectInclusionsField: TextField = 0
  lazy val rectInclusionsLabel: Label = "rect"

  lazy val nucleationField: TextField = 0
  lazy val nucleationLabel: Label = "seeds"

  lazy val mcInitializeButton: Button = "apply"
  lazy val mcInitializeField: TextField = 0
  lazy val mcInitializeLabel: Label = "states"
  lazy val mcIterationsButton: Button = "apply"
  lazy val mcIterationsField: TextField = 0
  lazy val mcIterationsLabel: Label = "iterations"

  lazy val neighbourhoodsBox: ComboBox[(String, Type)] = VonNeumann :: ExtendedMoore :: Moore :: Pentagonal :: Hexagonal :: Nil
  lazy val boundariesBox: ComboBox[(String, Type)] = Absorbs :: Periodic :: Nil
  lazy val spaceBox: ComboBox[(String, Type)] = CA :: MC :: SPX :: Nil
  lazy val nucleationBox: ComboBox[(String, Type)] = Lack :: Constant :: Increasing :: Decreasing :: Nil
  lazy val distributionBox: ComboBox[(String, Type)] = Heterogenous :: Homogenous :: Nil

  lazy val srxMenu = new GridPanel(1, 21) {
    contents ++= nucleationBox :: distributionBox :: Nil
    border = "SRX"
  }
  lazy val mcMenu = new GridPanel(2, 3) {
    contents ++= mcInitializeField :: mcInitializeLabel :: mcInitializeButton ::
      mcIterationsField :: mcIterationsLabel :: mcIterationsButton :: Nil
    border = "MC"
  }

  lazy val spaceMenu = new GridPanel(1, 2) {
    contents ++= spaceBox :: neighbourhoodsBox :: boundariesBox :: Nil
    border = "space"
  }

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

  lazy val menu = new BoxPanel(Vertical) {
    contents ++= inclusionsMenu :: nucleationMenu :: spaceMenu :: mcMenu :: srxMenu :: activity :: Nil
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
      mcInitializeButton,
      mcIterationsButton,
      spaceBox.selection,
      neighbourhoodsBox.selection,
      boundariesBox.selection,
      nucleationBox.selection,
      distributionBox.selection,
      canvas.mouse.clicks
    )

    reactions += {
      case SelectionChanged(`spaceBox`) |
           SelectionChanged(`neighbourhoodsBox`) |
           SelectionChanged(`boundariesBox`) |
           SelectionChanged(`nucleationBox`) |
           SelectionChanged(`distributionBox`) =>

        space = SpaceFactory(width, height)(spaceBox, neighbourhoodsBox, boundariesBox, nucleationBox, distributionBox)
      case ButtonClicked(`mcIterationsButton`) =>
        Future {
          canvas.iterate(space, Context(mcIterationsField))
        }
      case ButtonClicked(`mcInitializeButton`) =>
        canvas.generate(mcInitializeField)
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
