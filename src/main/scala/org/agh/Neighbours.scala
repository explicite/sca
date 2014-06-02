package org.agh

import java.awt.Color
import scala.annotation.switch

abstract class Neighbours
  extends Boundaries
  with ShapeControl {

  def onTheEdge(x: Int, y: Int)(implicit space: Seq[Cell]): Boolean = {
    val milieuValues = evaluate(milieu(x, y))
    val milieuSize = milieuValues.size
    val uniqueSize = milieuValues.distinct.size

    milieuSize != uniqueSize
  }

  val probability: Double

  protected def value(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    (mutate _ andThen expand)(coordinates(x, y))
  }

  protected def coordinates(x: Int, y: Int): Seq[(Int, Int)]

  private def milieu(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y) ::(x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

trait VonNeumann extends Neighbours with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y - 1) ::(x, y + 1) ::(x - 1, y) ::(x + 1, y) :: Nil
  }
}

trait NearestMoore extends Neighbours with Convexity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
  }
}

trait FurtherMoore extends Neighbours with Convexity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
  }
}

trait RandomMoore extends Neighbours with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (randomFloat: @switch) match {
      case f: Float if f <= probability =>
        (x, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x, y + 1) :: Nil
      case _ => Seq.empty
    }
  }
}

trait Moore extends Neighbours with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

trait Pentagonal extends Neighbours with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (randomCase(4): @switch) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) :: Nil
      case 2 => (x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
      case 3 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
      case 4 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
    }
  }
}

trait Hexagonal extends Neighbours with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (randomCase(2): @switch) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
      case 2 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) :: Nil
    }
  }
}

trait ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color]
}

trait Concavity extends ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color] = {
    (cells: @switch) match {
      case head :: tail =>
        val cellsWithCount = cells map (c => (c, cells.count(_ == c)))
        Some(cellsWithCount.sortWith(_._2 < _._2).map(ci => ci._1).head)
      case Nil => None
    }
  }
}

trait Convexity extends ShapeControl {
  protected def expand(cells: Seq[Color]): Option[Color] = {
    //TODO tail recursion
    def evaluate(s: Int): Option[Color] = {
      def chained(idx: Int): Boolean = {
        def bound(idx: Int) = cells(((idx % cells.length) + cells.length) % cells.length).getRGB

        if (bound(idx - 1) == bound(idx) && bound(idx - 1) == bound(idx + 1) && bound(idx) == bound(idx + 1))
          true
        else
          false
      }

      if (chained(s))
        Some(cells(s))
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
