package org.agh

import scala.annotation._
import java.awt.Color._

trait Space extends Neighbours {
  val permanent = BLACK :: WHITE :: Nil
  val probability = 0.7d

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
        case WHITE => c(value)
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
