package org.agh

import scala.annotation._
import scala.collection.breakOut

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract case class Space(width: Int, height: Int) extends Neighbours {

  import java.awt.Color._

  val inactive = BLACK :: WHITE :: Nil
  val probability = 0.7d

  def iterate(implicit space: Seq[Cell]): Seq[Cell] = {
    space.par.map (c => (c.v: @switch) match {
      case WHITE => c.evaluate(neighbours)
      case _ => c
    })(breakOut)
  }
}
