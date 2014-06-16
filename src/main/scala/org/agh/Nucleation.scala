package org.agh

trait Nucleation extends Distribution {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell]
}

object Nucleation {

  import scala.reflect.runtime.universe.typeOf

  val Constant = ("Constant", typeOf[Constant])
  val Increasing = ("Increasing", typeOf[Increasing])
  val Decreasing = ("Decreasing", typeOf[Decreasing])
}

trait Constant extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = cells
}

trait Increasing extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = cells
}

trait Decreasing extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = cells
}
