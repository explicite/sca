package org

import scala.language.implicitConversions
import java.awt.Color

package object agh {

  // implicit conversions
  implicit def CellToValue(c: Cell): Color = c.value
  implicit def CellToCoordinate(c: Cell): (Int, Int) = (c.x, c.y)

  // random
  private val RANDOM = new scala.util.Random()

  def randomCase = (cases: Int) => RANDOM.nextInt(cases) + 1

  def randomFloat = RANDOM.nextFloat()

  def randomInt = (max: Int) => RANDOM.nextInt(max)

  def randomCell = (cells: Seq[Cell]) => RANDOM.shuffle(cells).head

  def randomBoolean = RANDOM.nextBoolean()

}
