package org.agh

import scala.util.Random
import java.awt.Color

/**
 * @author Jan Paw
 *         Date: 3/16/14
 */
abstract class Neighbours extends Boundaries {

  import Neighbours.random

  val probability: Double

  protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color]

  protected def randomCase(cases: Int): Int = random.nextInt(cases) + 1

  protected def first(cells: Seq[Cell]): Option[Color] = {
    cells match {
      case head :: tail =>
        val cellsWithCount = cells map (c => (c, cells.count(_.v == c.v)))
        Some(cellsWithCount.sortWith(_._2 < _._2).map(ci => ci._1).head.v)
      case Nil => None
    }
  }

  protected def chain(cells: Seq[Cell]): Option[Color] = {
    //TODO
    ???
  }
}

trait VonNeumann extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(Seq((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))))
  }
}

trait NearestMoore extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(Seq((x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y))))
  }
}

trait FurtherMoore extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(Seq((x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1))))
  }
}

trait RandomMoore extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    import Neighbours._

    random.nextFloat() match {
      case f: Float if f <= probability => first(mutate(Seq((x, y - 1), (x + 1, y), (x - 1, y), (x, y + 1))))
      case _ => None
    }
  }
}

trait Moore extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(Seq((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1))))
  }
}

trait Pentagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(randomCase(4) match {
      case 1 => Seq((x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y))
      case 2 => Seq((x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1))
      case 3 => Seq((x - 1, y - 1), (x, y - 1), (x - 1, y), (x + 1, y + 1), (x, y + 1))
      case 4 => Seq((x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1))
    }))
  }
}

trait Hexagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first(mutate(randomCase(2) match {
      case 1 => Seq((x - 1, y - 1), (x, y - 1), (x - 1, y), (x + 1, y), (x + 1, y + 1), (x, y + 1))
      case 2 => Seq((x, y - 1), (x + 1, y - 1), (x + 1, y), (x - 1, y), (x - 1, y + 1), (x, y + 1))
    }))
  }
}

object Neighbours {
  val random = new Random()
}