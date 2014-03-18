package org.agh

import org.scalatest.{FunSuite, Matchers}

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
class SpaceTest extends FunSuite with Matchers {
  val width: Int = 10
  val height: Int = 20
  val neighbours: Seq[(Int, Int)] = List((-1, -1), (10, 20), (0, 0))
  val testSpace = scala.collection.mutable.Seq.fill(3 * 3)(0.0)
  testSpace(4) = 1

  test("Periodic boundaries test") {
    val space = new VonNeumannSpace(width, height) with Periodic
    val s: Seq[(Int, Int)] = space mutate neighbours

    s(0) should equal(9, 19)
    s(1) should equal(0, 0)
    s(2) should equal(0, 0)
  }

  test("Absorbs boundaries test") {
    val space = new VonNeumannSpace(width, height) with Absorbs
    val s: Seq[(Int, Int)] = space mutate neighbours

    s.length should equal(1)
  }

  test("Moore with absorbs") {
    val space = new MooreSpace(3, 3) with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0)
  }

  test("Moore with periodic") {
    val space = new MooreSpace(3, 3) with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0)
  }

  test("VonNeumann with absorbs") {
    val space = new VonNeumannSpace(3, 3) with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  }

  test("VonNeumann with periodic") {
    val space = new VonNeumannSpace(3, 3) with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
  }

  /*test("Pentagonal with absorbs") {
    val space = new PentagonalSpace(3, 3) with Absorbs
    space.iterate(testSpace.toSeq) count(x => x == 1.0) should equal(6)
  }

  test("Pentagonal with periodic") {
    val space = new PentagonalSpace(3, 3) with Periodic
    space.iterate(testSpace.toSeq) count(x => x == 1.0) should equal(6)
  }

  test("Hexagonal with absorbs") {
    val space = new HexagonalSpace(3, 3) with Absorbs
    space.iterate(testSpace.toSeq) count(x => x == 1.0) should equal(7)
  }

  test("Hexagonal with periodic") {
    val space = new HexagonalSpace(3, 3) with Periodic
    space.iterate(testSpace.toSeq) count(x => x == 1.0) should equal(7)
  }*/
}
