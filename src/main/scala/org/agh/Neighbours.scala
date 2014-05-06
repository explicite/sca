package org.agh

import scala.util.Random
import java.awt.Color
import scala.annotation.switch

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
    (cells: @switch) match {
      case head :: tail =>
        val cellsWithCount = cells map (c => (c, cells.count(_.v == c.v)))
        Some(cellsWithCount.sortWith(_._2 < _._2).map(ci => ci._1).head.v)
      case Nil => None
    }
  }

  protected def chain(cells: Seq[Cell]): Option[Color] = {

    def evaluate(s: Int): Option[Color] = {
      def chained(idx: Int): Boolean = {
        def bound(idx: Int) = cells(((idx % cells.length) + cells.length) % cells.length).v.getRGB

        if (bound(idx - 1) == bound(idx) && bound(idx - 1) == bound(idx + 1) && bound(idx) == bound(idx + 1))
          true
        else
          false
      }

      if (chained(s))
        Some(cells(s).v)
      else if (s > 0)
        evaluate(s - 1)
      else
        None
    }

    (cells.length: @switch) match {
      case 3 => evaluate(2)
      case 4 => evaluate(3)
      case _ => None
    }

  }
}

trait VonNeumann extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first {
      mutate {
        (x, y - 1) ::(x, y + 1) ::(x - 1, y) ::(x + 1, y) :: Nil
      }
    }
  }
}

trait NearestMoore extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    chain {
      mutate {
        (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
      }
    }
  }
}

trait FurtherMoore extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    chain {
      mutate {
        (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
      }
    }
  }
}

trait RandomMoore extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    import Neighbours._

    (random.nextFloat(): @switch) match {
      case f: Float if f <= probability =>
        first {
          mutate {
            (x, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x, y + 1) :: Nil
          }
        }
      case _ => None
    }
  }
}

trait Moore extends Neighbours {
  protected override def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first {
      mutate {
        (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
      }
    }
  }
}

trait Pentagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first {
      mutate {
        (randomCase(4): @switch) match {
          case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) :: Nil
          case 2 => (x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
          case 3 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
          case 4 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
        }
      }
    }
  }
}

trait Hexagonal extends Neighbours {
  override protected def neighbours(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    first {
      mutate {
        (randomCase(2): @switch) match {
          case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
          case 2 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) :: Nil
        }
      }
    }
  }
}

object Neighbours {
  val random = new Random()
}