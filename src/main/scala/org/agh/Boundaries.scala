package org.agh

import java.awt.Color
import java.awt.Color._

abstract class Boundaries {
  val width: Int
  val height: Int
  val permanent: Seq[Color] = BLACK :: WHITE :: Nil

  def mutate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Color] = {
    (transforms _ andThen removePermanent)(coordinates)
  }

  def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)]

  def removePermanent(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Color] = {
    def predicate(c: Color): Boolean = !permanent.contains(c)
    evaluate(coordinates) filter predicate
  }
  
  def evaluate(coordinates: Seq[(Int, Int)])(implicit space: Seq[Cell]): Seq[Color] = {
    def toColor(coordinate: (Int, Int))(implicit space: Seq[Cell]): Color = {
      coordinate match {
        case (x, y) => space(y + (height * x))
      }
    }
    coordinates map toColor
  }
}

object Boundaries {
  import scala.reflect.runtime.universe.typeOf
  val Absorbs = ("Absorbs", typeOf[Absorbs])
  val Periodic = ("Periodic", typeOf[Periodic])
}

trait Periodic extends Boundaries {
  override def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates map transform
  }

  private def transform(coordinate: (Int, Int)): (Int, Int) = {
    coordinate match {
      case (x, y) => (((x % width) + width) % width, ((y % height) + height) % height)
    }
  }
}

trait Absorbs extends Boundaries {
  override def transforms(coordinates: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    coordinates filter predicate
  }

  private def predicate(coordinate: (Int, Int)): Boolean = {
    coordinate match {
      case (x, y) => x >= 0 && y >= 0 && x < width && y < height
    }
  }
}