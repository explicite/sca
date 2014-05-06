package org.agh

import scala.annotation._

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract case class Space(width: Int, height: Int) extends Neighbours {

  import java.awt.Color._

  val inactive = BLACK :: WHITE :: Nil
  val probability = 0.7d

  /**
   * Standard CA iteration
   * 
   * @param space space to iterate
   * @return evaluated space
   */
  def iterate(implicit space: Seq[Cell]): Seq[Cell] = {
    space.map {
      c => (c.v: @switch) match {
        case WHITE => c.evaluate(neighbours)
        case _ => c
      }
    }
  }

  /**
   * Insert inclusions to space
   *
   * @param numberOfInclusions  on space
   * @param maxRadius  for inclusions
   * @param space with inclusions
   * @return
   */
  def setInclusions(numberOfInclusions: Int, maxRadius: Double)(implicit space: Seq[Cell]): Seq[Cell] = {
    ???
  }
}
