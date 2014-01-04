package cube

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CubeSuite extends FunSuite {

  trait SolutionChecker extends CubeDef with Solver {
    def testSize(size: Int, solution: List[Int]) {
      val sum = solution.sum
      assert(sum - (solution.size - 1) == Math.pow(size, 3), s"not $sum - ($size - 1)")
    }

    def getDistance(l: List[Int]): Int =
      l.min.abs + l.max.abs + 1

    def testDistance(s: Int, sol: List[Pos]) {
      assert(sol.size == Math.pow(s, 3), s"${sol.size} must be ${Math.pow(s, 3)}")
      assert(getDistance(sol.map(_.x)) == s, s"X: ${sol.map(_.x)} must be $s")
      assert(getDistance(sol.map(_.y)) == s, s"Y: ${sol.map(_.y)} must be $s")
      assert(getDistance(sol.map(_.z)) == s, s"Z: ${sol.map(_.z)} must be $s")
    }
  }

  trait Level0 extends SolutionChecker {
    val size = 1
    val pieces = List(1)
  }

  trait Level1 extends SolutionChecker {
    val size = 2
    val pieces = List(2, 2, 2, 2, 2, 2, 2)
  }

  trait Level2 extends SolutionChecker {
    val size = 3
    val pieces = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)
  }

  trait Level3 extends SolutionChecker {
    val size = 4
    val pieces = List(2, 4,
        2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 2, 4, 2, 2, 2, 4, 3, 2, 2,
        2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2,
        2, 2, 3, 2, 3, 2, 3, 2, 4, 2, 2, 3, 2, 3)
  }

  test("verify pieces for level0") {
    new Level0 {
      testSize(size, pieces)
    }
  }

  test("verify pieces for level1") {
    new Level1 {
      testSize(size, pieces)
    }
  }

  test("verify pieces for level2") {
    new Level2 {
      testSize(size, pieces)
    }
  }

  test("verify pieces for level3") {
    new Level3 {
      testSize(size, pieces)
    }
  }

  test("lastAxis testSuite") {
    new Level1 {
      assert(Cube(size, Stream()).lastAxis == ZAxis, "Default Axis failed")
      assert(Cube(size, Stream(zero.dx(1), zero)).lastAxis == XAxis, "XAxis failed")
      assert(Cube(size, Stream(zero.dy(-1), zero)).lastAxis == YAxis, "YAxis failed")
      assert(Cube(size, Stream(zero.dz(1), zero)).lastAxis == ZAxis, "ZAxis failed")
    }
  }

  test("neighbors are 4 good neighbors when Cube is empty") {
    new Level1 {
      val nb = Cube(size, Stream()).neighbors(2)
      assert(nb.size == 4, s"neighbors should be 4 not ${nb.size}")
      assert(Cube(size, nb(0)).lastAxis == XAxis)
      assert(Cube(size, nb(1)).lastAxis == XAxis)
      assert(Cube(size, nb(2)).lastAxis == YAxis)
      assert(Cube(size, nb(3)).lastAxis == YAxis)
    }
  }
  
  test("neighbors are 4 good neighbors") {
    new Level1 {
      val nb = Cube(size, Stream(zero.dx(1), zero)).neighbors(2)
      assert(nb.size == 4, s"neighbors should be 4 not ${nb.size}")
      assert(Cube(size, nb(0)).lastAxis == YAxis)
      assert(Cube(size, nb(1)).lastAxis == YAxis)
      assert(Cube(size, nb(2)).lastAxis == ZAxis)
      assert(Cube(size, nb(3)).lastAxis == ZAxis)

      val nb2 = Cube(size, Stream(zero.dy(1), zero)).neighbors(2)
      assert(nb2.size == 4, s"neighbors should be 4 not ${nb.size}")
      assert(Cube(size, nb2(0)).lastAxis == XAxis)
      assert(Cube(size, nb2(1)).lastAxis == XAxis)
      assert(Cube(size, nb2(2)).lastAxis == ZAxis)
      assert(Cube(size, nb2(3)).lastAxis == ZAxis)
    }
  }

  test("legalNeighbors returns 4 at first") {
    new Level1 {
      val nb = Cube(size, Stream()).legalNeighbors(2)
      assert(nb.size == 4, s"neighbors should be 4 not ${nb.size}")
      assert(Cube(size, nb(0)).lastAxis == XAxis)
      assert(Cube(size, nb(1)).lastAxis == XAxis)
      assert(Cube(size, nb(2)).lastAxis == YAxis)
      assert(Cube(size, nb(3)).lastAxis == YAxis)
    }
  }

  test("legalNeighbors returns only 2 neighbors in this case") {
    new Level1 {
      val c0 = zero
      val c1 = c0.dx(1)
      val c2 = c1.dy(1)
      val c3 = c2.dx(-1)
      val nb = Cube(size, Stream(c3, c2, c1, c0)).legalNeighbors(2)
      assert(nb.size == 2, s"neighbors should be 2 not ${nb.size}")
      assert(Cube(size, nb(0)).lastAxis == ZAxis)
      assert(Cube(size, nb(1)).lastAxis == ZAxis)
    }
  }

  test("validity of cube 1x1x1") {
    new Level0 {
      assert(Cube(size, Stream(zero)).isDone)
    }
  }

  test("validity of cube 2x2x2") {
    new Level1 {
      assert(Cube(size, Stream(
        Pos(0, 0, 1),
        Pos(1, 0, 1),
        Pos(1, 1, 1),
        Pos(0, 1, 1),
        Pos(0, 1, 0),
        Pos(1, 1, 0),
        Pos(1, 0, 0),
        Pos(0, 0, 0))).isDone)
    }
  }

  test("Cube 1") {
    new Level0 {
      testDistance(1, solution)
    }
  }

  test("Cube 2") {
    new Level1 {
      testDistance(2, solution)
    }
  }

  test("Cube 3") {
    new Level2 {
      testDistance(3, solution)
    }
  }
  
//    test("Cube 4") {
//    new Level3 {
//      testDistance(4, solution)
//    }
//  }
}