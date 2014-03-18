package org.agh

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
abstract class Boundaries {
  val width: Int
  val height: Int

  def mutate(neighbours: Seq[(Int, Int)]): Seq[(Int, Int)]
}

trait Periodic extends Boundaries {
  private def pX(x: Int): Int = ((x % width) + width) % width

  private def pY(y: Int): Int = ((y % height) + height) % height

  override def mutate(neighbours: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    neighbours.map(p => (pX(p._1), pY(p._2)))
  }
}

trait Absorbs extends Boundaries {
  override def mutate(neighbours: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    var tmp: Seq[(Int, Int)] = Nil

    for (position <- neighbours) {
      val x = position._1
      val y = position._2

      if (!(x < 0 || y < 0 || x >= width || y >= height))
        tmp ++= position :: Nil
    }

    tmp
  }
}