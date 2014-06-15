package org.agh

import java.awt.Color

import scala.annotation.switch

abstract class Neighbourhood
  extends Boundaries
  with ShapeControl {

  protected val probability: Double = 0.7d

  def isEdge(coordinate: (Int, Int))(implicit space: Seq[Cell]): Boolean = {
    // TODO skip black
    val (x, y) = coordinate
    val values = (transforms _ andThen evaluate)(milieu(x, y))
    val uniqueValues = values groupBy (_.getRGB)

    (uniqueValues.size: @switch) match {
      case 1 => false
      case _ => values.size != uniqueValues.size
    }
  }

  def edges(implicit space: Seq[Cell]): Seq[Cell] = {
    space filter (cell => isEdge(cell.x, cell.y))
  }

  protected def states(x: Int, y: Int)(implicit space: Seq[Cell]): Seq[Color] = {
    mutate(coordinates(x, y))
  }

  protected def value(x: Int, y: Int)(implicit space: Seq[Cell]): Option[Color] = {
    (mutate _ andThen expand)(coordinates(x, y))
  }

  protected def coordinates(x: Int, y: Int): Seq[(Int, Int)]

  private def milieu(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y) ::(x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

object Neighbourhood extends Enumeration {

  import scala.reflect.runtime.universe.typeOf

  val VonNeumann = ("Von Neumann", typeOf[VonNeumann])
  val NearestMoore = ("Nearest Moore", typeOf[NearestMoore])
  val FurtherMoore = ("Further Moore", typeOf[FurtherMoore])
  val RandomMoore = ("Random Moore", typeOf[RandomMoore])
  val Moore = ("Moore", typeOf[Moore])
  val Pentagonal = ("Pentagonal", typeOf[Pentagonal])
  val Hexagonal = ("Hexagonal", typeOf[Hexagonal])
}

trait VonNeumann extends Neighbourhood with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y - 1) ::(x, y + 1) ::(x - 1, y) ::(x + 1, y) :: Nil
  }
}

trait NearestMoore extends Neighbourhood with Convexity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x, y - 1) ::(x + 1, y) ::(x, y + 1) ::(x - 1, y) :: Nil
  }
}

trait FurtherMoore extends Neighbourhood with Convexity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x - 1, y - 1) ::(x + 1, y - 1) ::(x + 1, y + 1) ::(x - 1, y + 1) :: Nil
  }
}

trait RandomMoore extends Neighbourhood with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (randomFloat: @switch) match {
      case f: Float if f <= probability =>
        (x, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x, y + 1) :: Nil
      case _ => Seq.empty
    }
  }
}

trait Moore extends Neighbourhood with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
  }
}

trait Pentagonal extends Neighbourhood with Concavity {
  override protected def coordinates(x: Int, y: Int): Seq[(Int, Int)] = {
    (randomCase(4): @switch) match {
      case 1 => (x - 1, y - 1) ::(x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x - 1, y) :: Nil
      case 2 => (x + 1, y) ::(x - 1, y) ::(x - 1, y + 1) ::(x, y + 1) ::(x + 1, y + 1) :: Nil
      case 3 => (x - 1, y - 1) ::(x, y - 1) ::(x - 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
      case 4 => (x, y - 1) ::(x + 1, y - 1) ::(x + 1, y) ::(x + 1, y + 1) ::(x, y + 1) :: Nil
    }
  }
}

trait Hexagonal extends Neighbourhood with Concavity {
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