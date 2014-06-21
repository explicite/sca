package org.agh

case class NucleationContext(numberOfIterations: Int,
                             nucleation: Int,
                             edge: Double,
                             grain: Double,
                             current: Int = 0) {
  val end: Boolean = numberOfIterations == current

  val numberOfGerms = nucleation

  val constant = nucleation

  val onTheBeginning = current == 0 match {
    case true => nucleation
    case false => 0
  }

  val increasing = nucleation * current

  val decreasing = nucleation * (numberOfIterations - current)

  def ++ = NucleationContext(numberOfIterations, nucleation, edge, grain, current + 1)

  def -- = NucleationContext(numberOfIterations, nucleation, edge, grain, current - 1)
}

object NucleationContext {
  def One = NucleationContext(
    numberOfIterations = 1,
    nucleation = 0,
    edge = 0,
    grain = 0,
    current = 0)

  def Times(n: Int) = NucleationContext(
    numberOfIterations = n,
    nucleation = 0,
    edge = 0,
    grain = 0,
    current = 0)
}