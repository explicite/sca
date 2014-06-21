package org.agh

trait Distribution {
  def insertGerm(n: Int)(implicit cells: Seq[Cell], context: NucleationContext): Seq[Cell] = {
    //TODO dynamic calculate NOG
    val probabilityForEdge = context.edge
    val probabilityForGrain = context.grain
    update(inEdge((n * probabilityForEdge).toInt) ++ inGrain((n * probabilityForGrain).toInt))
  }

  def inEdge(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty
    val notRecrystallized = cells.filterNot(_.recrystallized)

    if (notRecrystallized.nonEmpty) {
      var itr = 0
      while (itr < n) {
        val cell = randomCell(notRecrystallized)
        if (onEdge(cell)) {
          grains ++= (cell ~ true ~ randomColor ~ 0) :: Nil
          itr += 1
        }
      }
    }

    grains
  }

  def inGrain(n: Int)(implicit cells: Seq[Cell]): Seq[Cell] = {
    var grains: Seq[Cell] = Seq.empty
    val notRecrystallized = cells.filterNot(_.recrystallized)

    if (notRecrystallized.size > cells.size * 0.05) {
      var itr = 0
      while (itr < n) {
        val cell = randomCell(notRecrystallized)
        if (!onEdge(cell)) {
          grains ++= (cell ~ true ~ randomColor ~ 0) :: Nil
          itr += 1
        }
      }
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
  override def insertGerm(n: Int)(implicit cells: Seq[Cell], context: NucleationContext): Seq[Cell] = {
    update(inEdge((n * 0.5).toInt) ++ inGrain((n * 0.5).toInt))
  }
}

trait Heterogenous extends Distribution