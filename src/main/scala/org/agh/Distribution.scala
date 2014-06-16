package org.agh

trait Distribution

object Distribution {

  import scala.reflect.runtime.universe.typeOf

  val Homogenous = ("Homogenous", typeOf[Homogenous])
  val Heterogenous = ("Heterogenous", typeOf[Heterogenous])
}

trait Homogenous extends Distribution

trait Heterogenous extends Distribution
