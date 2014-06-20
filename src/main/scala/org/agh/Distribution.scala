package org.agh

trait Distribution {
  val probabilityForEdge: Double
  val probabilityForGrain: Double

  def insertGerm(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    update(inEdge(n * probabilityForEdge) ++ inGrain(n * probabilityForGrain))
  }

  def insertEnergy(e: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    cells.par.map { cell =>
      if (onEdge(cell))
        cell ~ randomDouble(1)
      else
        cell ~ randomDouble(4)
    }.seq
  }

  def inEdge(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty

    while (grains.size < n) {
      val cell = randomCell
      if (onEdge(cell))
        grains ++= (cell ~ true ~ randomColor ~ 0) :: Nil
    }

    grains
  }

  def inGrain(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty
    while (grains.size < n) {
      val cell = randomCell
      if (!onEdge(cell))
        grains ++= (cell ~ true ~ randomColor ~ 0) :: Nil
    }
    grains
  }

  def onEdge(cell: Cell)(implicit space: Seq[Cell]): Boolean
}

object Distribution {

  import scala.reflect.runtime.universe.typeOf

  val Homogenous = ("Homogenous", typeOf[Homogenous])
  val Heterogenous = ("Heterogenous", typeOf[Heterogenous])
}

trait Homogenous extends Distribution {
  val probabilityForEdge: Double = 0.5
  val probabilityForGrain: Double = 0.5
}

trait Heterogenous extends Distribution {
  val probabilityForEdge: Double = 0.75
  val probabilityForGrain: Double = 0.25
}
