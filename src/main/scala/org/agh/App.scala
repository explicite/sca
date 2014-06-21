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
  val width = 200
  val height = 200
  val cellSize = 3
  implicit var space: Space = SpaceFactory(width, height)(CA, VonNeumann, Absorbs, Lack, Heterogenous)

  lazy val canvas = new SpacePanel(width, height, cellSize)

  lazy val iterate: Button = "iterate"

  lazy val edges: Button = "remove edges"

  lazy val active: Button = "remove active"
  lazy val inactive: Button = "remove inactive"

  lazy val seedingButton: Button = "apply"

  lazy val circleInclusionsButton: Button = "apply"
  lazy val circleInclusionsField: TextField = 0
  lazy val circleInclusionsLabel: Label = "circle"

  lazy val rectInclusionsButton: Button = "apply"
  lazy val rectInclusionsField: TextField = 0
  lazy val rectInclusionsLabel: Label = "rect"

  lazy val seedingField: TextField = 0
  lazy val seedingLabel: Label = "seeds"

  lazy val nucleationField: TextField = 0
  lazy val nucleationLabel: Label = "germs"
  lazy val nucleationGrainsField: TextField = 0.0
  lazy val nucleationGrainsLabel: Label = "grains"
  lazy val nucleationEdgeField: TextField = 0.0
  lazy val nucleationEdgeLabel: Label = "edges"

  lazy val mcInitializeButton: Button = "apply"
  lazy val mcInitializeField: TextField = 0
  lazy val mcInitializeLabel: Label = "states"
  lazy val mcIterationsButton: Button = "apply"
  lazy val iterationsField: TextField = 0
  lazy val mcIterationsLabel: Label = "iterations"

  lazy val neighbourhoodsBox: ComboBox[(String, Type)] = VonNeumann :: ExtendedMoore :: Moore :: Pentagonal :: Hexagonal :: Nil
  lazy val boundariesBox: ComboBox[(String, Type)] = Absorbs :: Periodic :: Nil
  lazy val spaceBox: ComboBox[(String, Type)] = CA :: MC :: SPX :: Nil
  lazy val nucleationBox: ComboBox[(String, Type)] = Lack :: Constant :: Increasing :: Decreasing :: OnTheBeginning :: Nil
  lazy val distributionBox: ComboBox[(String, Type)] = Heterogenous :: Homogenous :: Nil
  lazy val visualisationBox: ComboBox[(String, Boolean)] = ("Value", false) ::("Energy", true) :: Nil

  lazy val srxMenu = new GridPanel(1, 3) {
    contents ++= nucleationBox :: distributionBox :: visualisationBox :: Nil
    border = "SRX"
  }

  lazy val nucleationMenu = new GridPanel(3, 2) {
    contents ++= nucleationField :: nucleationLabel ::
      nucleationGrainsField :: nucleationGrainsLabel ::
      nucleationEdgeField :: nucleationEdgeLabel :: Nil
    border = "nucleation"
  }

  lazy val mcMenu = new GridPanel(2, 3) {
    contents ++= mcInitializeField :: mcInitializeLabel :: mcInitializeButton ::
      iterationsField :: mcIterationsLabel :: mcIterationsButton :: Nil
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

  lazy val seedingMenu = new GridPanel(1, 3) {
    contents ++= seedingField :: seedingLabel :: seedingButton :: Nil
    border = "seeding"
  }

  lazy val activity = new GridPanel(2, 2) {
    contents ++= iterate :: edges :: active :: inactive :: Nil
    border = "activity"
  }

  lazy val menu = new BoxPanel(Vertical) {
    contents ++=
      inclusionsMenu ::
        seedingMenu ::
        spaceMenu ::
        mcMenu ::
        srxMenu ::
        nucleationMenu ::
        activity :: Nil
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
      seedingButton,
      mcInitializeButton,
      mcIterationsButton,
      spaceBox.selection,
      neighbourhoodsBox.selection,
      boundariesBox.selection,
      nucleationBox.selection,
      distributionBox.selection,
      visualisationBox.selection,
      canvas.mouse.clicks
    )

    reactions += {
      case SelectionChanged(`spaceBox`) |
           SelectionChanged(`neighbourhoodsBox`) |
           SelectionChanged(`boundariesBox`) |
           SelectionChanged(`nucleationBox`) |
           SelectionChanged(`distributionBox`) =>

        space = SpaceFactory(width, height)(spaceBox, neighbourhoodsBox, boundariesBox, nucleationBox, distributionBox)
      case SelectionChanged(`visualisationBox`) =>
        canvas.repaint(visualisationBox)
      case ButtonClicked(`mcIterationsButton`) =>
        Future {
          canvas.iterate(visualisationBox)(
            space,
            NucleationContext(
              numberOfIterations = iterationsField,
              nucleation = nucleationField,
              edge = nucleationEdgeField,
              grain = nucleationGrainsField
            )
          )
        }
      case ButtonClicked(`mcInitializeButton`) =>
        canvas.generate(mcInitializeField)
      case ButtonClicked(`iterate`) =>
        canvas.iterate(visualisationBox)(
          space,
          NucleationContext(
            numberOfIterations = 1,
            nucleation = nucleationField,
            edge = nucleationEdgeField,
            grain = nucleationGrainsField
          )
        )
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
      case ButtonClicked(`seedingButton`) =>
        canvas.setNucleation(seedingField)
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
