package org.agh

import org.scalatest.{FunSuite, Matchers}

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
class SpaceTest extends FunSuite with Matchers with Speed {
 /* val width: Int = 10
  val height: Int = 20
  val v = 0.1f
  val neighbours: Seq[(Int, Int)] = List((-1, -1), (10, 20), (0, 0))
  val testSpace = scala.collection.mutable.Seq.fill(3 * 3)(0.0f)
  testSpace(4) = v

  test("Periodic boundaries test") {
    val space = new Space(width, height) with Moore with Periodic
    val s: Seq[(Int, Int)] = space mutate neighbours

    s(0) should equal(9, 19)
    s(1) should equal(0, 0)
    s(2) should equal(0, 0)
  }

  test("Absorbs boundaries test") {
    val space = new Space(width, height) with Moore with Absorbs
    val s: Seq[(Int, Int)] = space mutate neighbours

    s.length should equal(1)
  }

  test("Neares Moore with absorbs") {
    val space = new Space(3, 3) with NearestMoore with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(Color.WHITE, v, Color.WHITE, v, v, v, Color.WHITE, v, Color.WHITE)
  }

  test("Nearest Moore with periodic") {
    val space = new Space(3, 3) with NearestMoore with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(0.0f, v, 0.0f, v, v, v, 0.0f, v, 0.0f)
  }

  test("Further Moore with absorbs") {
    val space = new Space(3, 3) with FurtherMoore with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(v, 0.0f, v, 0.0f, v, 0.0f, v, 0.0f, v)
  }

  test("Further Moore with periodic") {
    val space = new Space(3, 3) with FurtherMoore with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(v, 0.0f, v, 0.0f, v, 0.0f, v, 0.0f, v)
  }

  test("Moore with absorbs") {
    val space = new Space(3, 3) with Moore with Absorbs
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(v, v, v, v, v, v, v, v, v)
  }

  test("Moore with periodic") {
    val space = new Space(3, 3) with Moore with Periodic
    space.iterate(testSpace.toSeq) should contain theSameElementsAs List(v, v, v, v, v, v, v, v, v)
  }

  test("Iteration speed test") {
    val width = 10000
    val height = 10000

    var z1 = 0
    time("while loop") {
      var x: Int = 0
      var y: Int = 0

      while (x < width) {
        while (y < height) {
          z1 += x + y
          y += 1
        }
        x += 1
        y = 0
      }
    }

    var z2 = 0
    time("for loop optimized") {
      import scalaxy.loops._

      for (x <- (0 until width).optimized) {
        for (y <- (0 until height).optimized) {
          z2 += x + y
        }
      }
    }

    z1 should equal(z2)
  }*/
}


trait Speed {
  def time[T](str: String)(thunk: => T): T = {
    print(str + "... ")
    val t1 = System.currentTimeMillis
    val x = thunk
    val t2 = System.currentTimeMillis
    println((t2 - t1) + " msecs")
    x
  }
}