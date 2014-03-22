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
  val testSpace = scala.collection.mutable.Seq.fill(3 * 3)(0.0f)
  testSpace(4) = 1

  test("Periodic boundaries test") {
    val space = new Space(width, height) with VonNeumann with Periodic
    val s: Seq[(Int, Int)] = space mutate neighbours

    s(0) should equal(9, 19)
    s(1) should equal(0, 0)
    s(2) should equal(0, 0)
  }

  test("Absorbs boundaries test") {
    val space = new Space(width, height) with VonNeumann with Absorbs
    val s: Seq[(Int, Int)] = space mutate neighbours

    s.length should equal(1)
  }

  test("Moore with absorbs") {
    val space = new Space(3, 3) with Moore with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(0.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f, 0.0f)
  }

  test("Moore with periodic") {
    val space = new Space(3, 3) with Moore with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(0.0f, 1.0f, 0.0f, 1.0f, 1.0f, 1.0f, 0.0f, 1.0f, 0.0f)
  }

  test("VonNeumann with absorbs") {
    val space = new Space(3, 3) with VonNeumann with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f)
  }

  test("VonNeumann with periodic") {
    val space = new Space(3, 3) with VonNeumann with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f)
  }
}
