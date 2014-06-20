package org.agh

trait Nucleation extends Distribution {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell]
}

object Nucleation {

  import scala.reflect.runtime.universe.typeOf

  val Constant = ("Constant", typeOf[Constant])
  val Increasing = ("Increasing", typeOf[Increasing])
  val Decreasing = ("Decreasing", typeOf[Decreasing])
  val Lack = ("Lack", typeOf[Lack])
}

trait Constant extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = {
    insertEnergy(10.0)(insertGerm(5))
  }
}

trait Increasing extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = {
    insertEnergy(10.0)(insertGerm(5))
  }
}

trait Decreasing extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = {
    insertEnergy(10.0)(insertGerm(5))
  }
}

trait Lack extends Nucleation {
  def nucleation(implicit cells: Seq[Cell]): Seq[Cell] = {
    insertEnergy(10.0)
  }
}
