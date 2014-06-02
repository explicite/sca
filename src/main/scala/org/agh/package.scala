package org

import java.awt.Color

package object agh {
  implicit def CellToValue(c: Cell): Color = c.v
  private val RANDOM = new scala.util.Random()

  def randomCase = (cases: Int) => RANDOM.nextInt(cases) + 1

  def randomFloat = RANDOM.nextFloat()

  def randomInt = (max: Int) => RANDOM.nextInt(max)

  def randomCell = (cells: Seq[Cell]) => RANDOM.shuffle(cells).head

  def randomBoolean = RANDOM.nextBoolean()

}
