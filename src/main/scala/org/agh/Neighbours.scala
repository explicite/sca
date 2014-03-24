package org.agh

import scala.util.Random

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract class Neighbours extends Boundaries {

  import Neighbours.random

  protected def neighbours(x: Int, y: Int): Seq[(Int, Int)]

  protected def randomCase(cases: Int): Int = random.nextInt(cases) + 1
}

trait Moore extends Neighbours {
  protected override def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(Seq((x, y - 1), (x + 1, y), (x - 1, y), (x, y + 1)))
  }
}

trait VonNeumann extends Neighbours {
  protected override def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(Seq((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)))
  }
}

trait Pentagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(randomCase(4) match {
      case 1 => Seq((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y))
      case 2 => Seq((x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1))
      case 3 => Seq((x - 1, y - 1), (x, y - 1), (x - 1, y), (x + 1, y + 1), (x, y + 1))
      case 4 => Seq((x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1))
    })
  }

}

trait Hexagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int): Seq[(Int, Int)] = {
    mutate(randomCase(2) match {
      case 1 => Seq((x - 1, y - 1), (x, y - 1), (x - 1, y), (x + 1, y), (x + 1, y + 1), (x, y + 1))
      case 2 => Seq((x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1))
    })
  }
}

object Neighbours {
  val random = new Random()
}