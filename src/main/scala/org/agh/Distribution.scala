package org.agh

trait Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell]

  def onEdge(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty

    while (grains.size < n) {
      val cell = randomCell
      if (edge(cell))
        grains ++= cell ~ randomColor ~ true :: Nil
    }

    grains.seq
  }

  def onGrain(n: Double)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty
    while (grains.size < n) {
      val cell = randomCell
      if (!edge(cell))
        grains ++= cell ~ randomColor ~ true :: Nil
    }
    grains.seq
  }

  def edge(coordinate: (Int, Int))(implicit space: Seq[Cell]): Boolean
}

object Distribution {

  import scala.reflect.runtime.universe.typeOf

  val Homogenous = ("Homogenous", typeOf[Homogenous])
  val Heterogenous = ("Heterogenous", typeOf[Heterogenous])
}

trait Homogenous extends Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    update(onEdge(n * 0.5) ++ onGrain(n * 0.5))
  }
}

trait Heterogenous extends Distribution {
  def insert(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    update(onEdge(n * 0.75) ++ onGrain(n * 0.25))
  }
}
