package org.agh

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract case class Space(width: Int, height: Int) extends Neighbours {

  import java.awt.Color._

  val inactive = BLACK :: WHITE :: Nil
  val probability = 0.7d

  def iterate(implicit space: Seq[Cell]): Seq[Cell] = {
    space map (c => c.v match {
      case WHITE => c.evaluate(neighbours)
      case _ => c
    })
  }
}
