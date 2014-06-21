package org.agh
package view

import java.awt.Color._
import java.awt.{Color, Dimension, Graphics}
import javax.swing.JComponent

import scala.collection.breakOut
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing.Component
import scala.swing.event.MouseClicked

class SpacePanel(val width: Int, val height: Int, cellSize: Int)
  extends Component
  with Inclusions {

  implicit var cells: Seq[Cell] = empty()
  var paintEnergy = false

  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      var i = 0
      while (i < cells.length) {
        val c = cells(i)
        if (paintEnergy)
          g.setColor(if(c.energy == 0) BLUE else RED)
        else
          g.setColor(c)
        g.fillRect(c.x * cellSize, c.y * cellSize, cellSize, cellSize)
        i += 1
      }
    }

    override def update(g: Graphics) = {
      val offScreen = createImage(width * cellSize, height * cellSize)
      val offGraphics = offScreen.getGraphics

      paint(offGraphics)
      g.drawImage(offScreen, 0, 0, this)
    }
  }

  def repaint(energy: Boolean): Unit = {
    paintEnergy = energy
    repaint()
  }

  def iterate(energy: Boolean = false)(implicit space: Space, context: NucleationContext) {
    if (context.end)
      println("END")
    else {
      cells = space.iterate
      paintEnergy = energy
      repaint()
      iterate(energy)(space, context ++)
    }
  }

  def onTheEdge(modify: Cell => Cell)(implicit space: Space) {
    cells = space.onTheEdge(modify)
    repaint()
  }

  def selectGrain(mc: MouseClicked)(implicit space: Space) {
    val cell = getCell(mc)
    def modifier(c: Cell): Cell = {
      c.value match {
        case cell.value => Cell(c.x, c.y, BLACK)
        case _ => c
      }
    }
    cells = space.modify(modifier)
    repaint()
  }

  def removeActive() {
    cells = cells.map {
      cell =>
        cell.value match {
          case BLACK => cell
          case _ => Cell(cell.x, cell.y)
        }
    }

    repaint()
  }

  def removeInactive() {
    cells = cells.map {
      cell =>
        cell.value match {
          case BLACK => Cell(cell.x, cell.y)
          case _ => cell
        }
    }

    repaint()
  }

  // TODO fo cell size  > 1
  def insertRandom(clicked: MouseClicked) {
    val x0 = clicked.point.x
    val y0 = clicked.point.y
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      Future {
        if (x == x0 && y == y0)
          Cell(x, y, randomColor)
        else
          cells(y + (height * x))
      }
    }

    cells = futures.map(c => Await.result(c, 100 milli))(breakOut)

    repaint()
  }

  def setNucleation(numberOfSeeds: Int)(implicit space: Space): Unit = {
    implicit val spaceWithSeeds = scala.collection.mutable.Seq(cells: _*)

    for (seed <- 0 until numberOfSeeds) {
      randomCell match {
        case cell =>
          spaceWithSeeds(cell.y + (cell.x * space.height)) = cell ~ randomColor
      }
    }

    cells = spaceWithSeeds

    repaint()
  }

  def getCell(clicked: MouseClicked): Cell = {
    val point = clicked.point
    cells(point.y + (height * point.x))
  }

  def generate(numberOfStates: Int): Unit = {
    val states: scala.collection.mutable.Set[Color] = scala.collection.mutable.Set.empty

    while (states.size < numberOfStates) {
      states += randomColor
      states.remove(WHITE)
      states.remove(BLACK)
    }

    val values = states.toSeq

    cells = cells.map {
      cell =>
        cell.value match {
          case WHITE => cell ~ RANDOM.shuffle(values).head
          case _ => cell
        }
    }

    repaint()
  }

  private def empty(): Seq[Cell] = {
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      Future {
        Cell(x, y)
      }
    }

    futures.map(c => Await.result(c, 100 milli))(breakOut)
  }
}
