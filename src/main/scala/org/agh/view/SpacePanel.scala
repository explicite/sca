package org.agh
package view

import scala.concurrent.ExecutionContext.Implicits.global
import java.awt.{Dimension, Graphics}
import scala.swing.event.MouseClicked
import scala.concurrent.duration._
import scala.collection.breakOut
import scala.language.postfixOps
import scala.annotation.switch
import javax.swing.JComponent
import scala.swing.Component
import scala.concurrent._
import java.awt.Color._
import java.awt.Color
import org.agh.Cell

class SpacePanel(val width: Int, val height: Int, cellSize: Int)
  extends Component
  with Inclusions {

  implicit var cells: Seq[Cell] = Nil

  override lazy val peer = new JComponent {
    setPreferredSize(new Dimension(width * cellSize, height * cellSize))

    override def paint(g: Graphics) = {
      var i = 0
      while (i < cells.length) {
        val c = cells(i)
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

  def iterate(implicit space: Space) {
    cells = space.iterate
    repaint()
  }

  def onTheEdge(modify: Cell => Cell)(implicit space: Space) {
    cells = space.onTheEdge(modify)
    repaint()
  }

  def selectGrain(mc: MouseClicked)(implicit space: Space) = {
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

  def getCell(mc: MouseClicked): Cell = {
    val point = mc.point
    cells(point.y + (height * point.x))
  }

  /**
   * Generate space
   *
   * @param seeds number of nucleation cells
   * @param inclusions number of inactive cells
   */
  def generate(seeds: Float = 1f, inclusions: Float = 1f): Unit = {
    val futures = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      Future {
        Cell(x, y, (randomFloat: @switch) match {
          case x: Float if x > seeds => if (x > inclusions) BLACK else randomColor
          case _ => WHITE
        })
      }
    }

    cells = futures.map(c => Await.result(c, 100 milli))(breakOut)
  }
}
