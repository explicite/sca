package org.agh

trait Distribution {
  def insertEnergy(cells: Seq[Cell]): Seq[Cell]

  def insertGerm(n: Int)(implicit cells: Seq[Cell], context: NucleationContext): Seq[Cell] = {
    val withEnergy = insertEnergy(cells)
    update(inEdge((n * context.edge).toInt) ++ inGrain((n * context.grain).toInt))(withEnergy)
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

trait Homogenous extends Distribution   {
  def insertEnergy(cells: Seq[Cell]): Seq[Cell] = {
    cells.par.map {
      cell =>
        if(!cell.recrystallized) {
          cell ~ 0
        } else cell
    }.seq
  }
}


trait Heterogenous extends Distribution  {
  def insertEnergy(cells: Seq[Cell]): Seq[Cell] = {
    cells.par.map {
      cell =>
        if(!cell.recrystallized) {
          if (onEdge(cell)(cells)) {
            cell ~ randomDouble(5)
          } else cell ~ -randomDouble(5)
        } else cell
    }.seq
  }
}