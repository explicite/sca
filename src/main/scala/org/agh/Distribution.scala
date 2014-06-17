package org.agh

trait Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell]

  def inEdge(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty

    while (grains.size < n) {
      val cell = randomCell
      if (onEdge(cell))
        grains ++= cell ~ randomColor ~ true :: Nil
    }

    grains.seq
  }

  def inGrain(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty
    while (grains.size < n) {
      val cell = randomCell
      if (!onEdge(cell))
        grains ++= cell ~ randomColor ~ true :: Nil
    }
    grains.seq
  }

  def onEdge(cell: Cell)(implicit space: Seq[Cell]): Boolean
}

object Distribution {

  import scala.reflect.runtime.universe.typeOf

  val Homogenous = ("Homogenous", typeOf[Homogenous])
  val Heterogenous = ("Heterogenous", typeOf[Heterogenous])
}

trait Homogenous extends Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    update(inEdge(n * 0.5) ++ inGrain(n * 0.5))
  }
}

trait Heterogenous extends Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    update(inEdge(n * 0.75) ++ inGrain(n * 0.25))
  }
}
