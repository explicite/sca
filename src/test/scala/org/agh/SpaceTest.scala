package org.agh

import org.scalatest.{FunSuite, Matchers}
import java.awt.Color._

/**
 * @author Jan Paw
 *         Date: 3/17/14
 */
class SpaceTest extends FunSuite with Matchers with Speed {
  val width: Int = 10
  val height: Int = 20

  val testSpace = Seq(
    Cell(0, 0, WHITE) , Cell(0, 1, WHITE) , Cell(0, 2, WHITE) ,
    Cell(1, 0, RED) , Cell(1, 1, WHITE) , Cell(1, 2, WHITE) ,
    Cell(2, 0, WHITE) , Cell(2, 1, WHITE) , Cell(2, 2, WHITE))

  test("Von Neumann with absorbs") {
    val space = new Space(3, 3) with VonNeumann with Absorbs
    space iterate testSpace map(_.v) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      RED, RED, WHITE,
      RED, WHITE, WHITE)
  }

  test("Von Neumann with periodic") {
    val space = new Space(3, 3) with VonNeumann with Periodic
    space iterate testSpace map(_.v) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      RED, RED, RED,
      RED, WHITE, WHITE)
  }

  test("Neares Moore with absorbs") {
    val space = new Space(3, 3) with NearestMoore with Absorbs

    val s1 = Seq(
      Cell(0, 0, WHITE), Cell(0, 1, RED), Cell(0, 2, WHITE),
      Cell(1, 0, WHITE), Cell(1, 1, WHITE), Cell(1, 2, RED),
      Cell(2, 0, WHITE), Cell(2, 1, RED), Cell(2, 2, WHITE)
    )

    val s2 = Seq(
      Cell(0, 0, WHITE), Cell(0, 1, RED), Cell(0, 2, WHITE),
      Cell(1, 0, RED), Cell(1, 1, WHITE), Cell(1, 2, RED),
      Cell(2, 0, WHITE), Cell(2, 1, WHITE), Cell(2, 2, WHITE)
    )

    val s3 = Seq(
      Cell(0, 0, WHITE), Cell(0, 1, RED), Cell(0, 2, WHITE),
      Cell(1, 0, RED), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
      Cell(2, 0, WHITE), Cell(2, 1, RED), Cell(2, 2, WHITE)
    )

    val s4 = Seq(
      Cell(0, 0, WHITE), Cell(0, 1, WHITE), Cell(0, 2, WHITE),
      Cell(1, 0, RED), Cell(1, 1, WHITE), Cell(1, 2, RED),
      Cell(2, 0, WHITE), Cell(2, 1, RED), Cell(2, 2, WHITE)
    )

    space iterate s1  map(_.v) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      WHITE, RED, RED,
      WHITE, RED, WHITE
    )

    space iterate s2 map(_.v) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      RED, RED, RED,
      WHITE, WHITE, WHITE
    )

    space iterate s3 map(_.v) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      RED, RED, WHITE,
      WHITE, RED, WHITE
    )

    space iterate s4 map(_.v) should contain theSameElementsAs Seq(
      WHITE, WHITE, WHITE,
      RED, RED, RED,
      WHITE, RED, WHITE
    )
  }

  test("Further Moore with absorbs") {
    val space = new Space(3, 3) with FurtherMoore with Absorbs

    val s1 = Seq(
      Cell(0, 0, RED), Cell(0, 1, WHITE), Cell(0, 2, RED),
      Cell(1, 0, WHITE), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
      Cell(2, 0, RED), Cell(2, 1, WHITE), Cell(2, 2, WHITE)
    )

    val s2 = Seq(
      Cell(0, 0, RED), Cell(0, 1, WHITE), Cell(0, 2, RED),
      Cell(1, 0, WHITE), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
      Cell(2, 0, WHITE), Cell(2, 1, WHITE), Cell(2, 2, RED)
    )

    val s3 = Seq(
      Cell(0, 0, WHITE), Cell(0, 1, WHITE), Cell(0, 2, RED),
      Cell(1, 0, WHITE), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
      Cell(2, 0, RED), Cell(2, 1, WHITE), Cell(2, 2, RED)
    )

    val s4 = Seq(
      Cell(0, 0, RED), Cell(0, 1, WHITE), Cell(0, 2, WHITE),
      Cell(1, 0, WHITE), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
      Cell(2, 0, RED), Cell(2, 1, WHITE), Cell(2, 2, RED)
    )

    space iterate s1  map(_.v) should contain theSameElementsAs Seq(
      RED, WHITE, RED,
      WHITE, RED, WHITE,
      RED, WHITE, WHITE)

    space iterate s2  map(_.v) should contain theSameElementsAs Seq(
      RED, WHITE, RED,
      WHITE, RED, WHITE,
      WHITE, WHITE, RED)

    space iterate s3  map(_.v) should contain theSameElementsAs Seq(
      WHITE, WHITE, RED,
      WHITE, RED, WHITE,
      RED, WHITE, RED)

    space iterate s4  map(_.v) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      WHITE, RED, WHITE,
      RED, WHITE, RED)
  }

  test("Moore with absorbs") {
    val space = new Space(3, 3) with Moore with Absorbs
    space iterate testSpace.toSeq map(_.v) should contain theSameElementsAs Seq(
      RED, RED, WHITE,
      RED, RED, WHITE,
      RED, RED, WHITE)
  }

  test("Moore with periodic") {
    val space = new Space(3, 3) with Moore with Periodic
    space iterate testSpace map(_.v) should contain theSameElementsAs Seq(
      RED, RED, RED,
      RED, RED, RED,
      RED, RED, RED)
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
  }
}

trait Speed {
  def time[T](str: String)(thunk: => T): T = {
    print(str + "... ")
    val start = System.currentTimeMillis
    val x = thunk
    val stop = System.currentTimeMillis
    println((stop - start) + "[msec]")
    x
  }
}