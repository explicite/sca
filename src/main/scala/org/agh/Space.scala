package org.agh

import scala.annotation._
import java.awt.Color._
import scala.util.Random

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
trait Space extends Neighbours {

  import java.awt.Color._

  val inactive = BLACK :: WHITE :: Nil
  val probability = 0.7d
  val RANDOM = new Random(System.currentTimeMillis())

  /**
   * Iteration over all cells in space
   *
   * @param space space to iterate
   * @return evaluated space
   */
  def iterate(implicit space: Seq[Cell]): Seq[Cell]
}

abstract case class CASpace(width: Int, height: Int) extends Space {

  /**
   * Iteration over all cells in CA
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
}

abstract case class MASpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in Monte Carlo
   *
   * @param space space to iterate
   * @return evaluated space
   */
  override def iterate(implicit space: Seq[Cell]): Seq[Cell] = ???
}

abstract case class SRXSpace(width: Int, height: Int) extends Space {
  /**
   * Iteration over all cells in SRX
   *
   * @param space space to iterate
   * @return evaluated space
   */
  override def iterate(implicit space: Seq[Cell]): Seq[Cell] = ???
}
