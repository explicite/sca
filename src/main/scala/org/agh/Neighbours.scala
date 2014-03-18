package org.agh

import scala.util.Random


/**
 * @author Jan 
 *         Date: 3/16/14
 */
abstract class Neighbours extends Boundaries {

  import Neighbours._

  protected def neighbours(x: Int, y: Int): Seq[(Int, Int)]

  protected def randomCase(cases: Int): Int = random.nextInt(cases) + 1
}

trait Moore extends Neighbours {
  protected override def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate((x, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x, y + 1) ::(x, y) :: Nil)
  }
}

trait VonNeumann extends Neighbours {
  protected override def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate((x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) ::(x, y) :: Nil)
  }
}

trait Pentagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(randomCase(4) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x, y) :: Nil
      case 2 => (x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) ::(x, y) :: Nil
      case 3 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y + 1) ::(x, y + 1) ::(x, y) :: Nil
      case 4 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) ::(x, y) :: Nil
    })
  }

}

trait Hexagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(randomCase(2) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) ::(x, y) :: Nil
      case 2 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x, y) :: Nil
    })
  }

}

object Neighbours {
  val random = new Random()
}