package org.agh

trait Nucleation extends Distribution {
  def numberOfGerms(implicit context: NucleationContext): Int

  def nucleation(implicit cells: Seq[Cell], context: NucleationContext): Seq[Cell] = {
    insertGerm(numberOfGerms)
  }
}

object Nucleation {

  import scala.reflect.runtime.universe.typeOf

  val OnTheBeginning = ("OnTheBeginning", typeOf[OnTheBeginning])
  val Constant = ("Constant", typeOf[Constant])
  val Increasing = ("Increasing", typeOf[Increasing])
  val Decreasing = ("Decreasing", typeOf[Decreasing])
  val Lack = ("Lack", typeOf[Lack])
}

trait Constant extends Nucleation {
  def numberOfGerms(implicit context: NucleationContext): Int = {
      context.constant
  }
}

trait Increasing extends Nucleation {
  def numberOfGerms(implicit context: NucleationContext): Int = {
      context.increasing
  }
}

trait Decreasing extends Nucleation {
  def numberOfGerms(implicit context: NucleationContext): Int = {
     context.increasing
  }
}

trait OnTheBeginning extends Nucleation {
  def numberOfGerms(implicit context: NucleationContext): Int = {
    context.onTheBeginning
  }
}

trait Lack extends Nucleation {
  def numberOfGerms(implicit context: NucleationContext): Int = {
    0
  }

  override def nucleation(implicit cells: Seq[Cell], context: NucleationContext): Seq[Cell] = {
    cells
  }
}
