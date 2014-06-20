package org.agh

import java.awt.Color._

import org.scalatest.{FunSuite, Matchers}

class SpaceTest extends FunSuite with Matchers {
  val width: Int = 10
  val height: Int = 20

  val testSpace = Seq(
    Cell(0, 0, WHITE), Cell(0, 1, WHITE), Cell(0, 2, WHITE),
    Cell(1, 0, RED), Cell(1, 1, WHITE), Cell(1, 2, WHITE),
    Cell(2, 0, WHITE), Cell(2, 1, WHITE), Cell(2, 2, WHITE))

  test("Von Neumann with absorbs") {
    val space = new CASpace(3, 3) with VonNeumann with Absorbs with Lack with Homogenous
    space iterate testSpace map (_.value) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      RED, RED, WHITE,
      RED, WHITE, WHITE)
  }

  test("Von Neumann with periodic") {
    val space = new CASpace(3, 3) with VonNeumann with Periodic with Lack with Homogenous
    space iterate testSpace map (_.value) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      RED, RED, RED,
      RED, WHITE, WHITE)
  }

  test("Neares Moore with absorbs") {
    val space = new CASpace(3, 3) with NearestMoore with Absorbs with Lack with Homogenous

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

    space iterate s1 map (_.value) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      WHITE, RED, RED,
      WHITE, RED, WHITE
    )

    space iterate s2 map (_.value) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      RED, RED, RED,
      WHITE, WHITE, WHITE
    )

    space iterate s3 map (_.value) should contain theSameElementsAs Seq(
      WHITE, RED, WHITE,
      RED, RED, WHITE,
      WHITE, RED, WHITE
    )

    space iterate s4 map (_.value) should contain theSameElementsAs Seq(
      WHITE, WHITE, WHITE,
      RED, RED, RED,
      WHITE, RED, WHITE
    )
  }

  test("Further Moore with absorbs") {
    val space = new CASpace(3, 3) with FurtherMoore with Absorbs with Lack with Homogenous

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

    space iterate s1 map (_.value) should contain theSameElementsAs Seq(
      RED, WHITE, RED,
      WHITE, RED, WHITE,
      RED, WHITE, WHITE)

    space iterate s2 map (_.value) should contain theSameElementsAs Seq(
      RED, WHITE, RED,
      WHITE, RED, WHITE,
      WHITE, WHITE, RED)

    space iterate s3 map (_.value) should contain theSameElementsAs Seq(
      WHITE, WHITE, RED,
      WHITE, RED, WHITE,
      RED, WHITE, RED)

    space iterate s4 map (_.value) should contain theSameElementsAs Seq(
      RED, WHITE, WHITE,
      WHITE, RED, WHITE,
      RED, WHITE, RED)
  }

  test("Moore with absorbs") {
    val space = new CASpace(3, 3) with Moore with Absorbs with Lack with Homogenous
    space iterate testSpace map (_.value) should contain theSameElementsAs Seq(
      RED, RED, WHITE,
      RED, RED, WHITE,
      RED, RED, WHITE)
  }

  test("Moore with periodic") {
    val space = new CASpace(3, 3) with Moore with Periodic with Lack with Homogenous
    space iterate testSpace map (_.value) should contain theSameElementsAs Seq(
      RED, RED, RED,
      RED, RED, RED,
      RED, RED, RED)
  }

  test("Identification cells on the edge") {
    val space = new CASpace(3, 3) with Moore with Absorbs with Lack with Homogenous
    space.onEdge(testSpace(0))(testSpace) shouldBe false
    space.onEdge(testSpace(1))(testSpace) shouldBe true
    space.onEdge(testSpace(2))(testSpace) shouldBe false
    space.onEdge(testSpace(3))(testSpace) shouldBe true
    space.onEdge(testSpace(4))(testSpace) shouldBe true
    space.onEdge(testSpace(5))(testSpace) shouldBe false
    space.onEdge(testSpace(6))(testSpace) shouldBe true
    space.onEdge(testSpace(7))(testSpace) shouldBe true
    space.onEdge(testSpace(8))(testSpace) shouldBe false
  }
}