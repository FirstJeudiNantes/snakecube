package cube

object Main extends App {
  abstract class Level extends Solver

  object Level0 extends Level {
    val size = 1
    val pieces = List(1)
  }

  var sols: Stream[Any] = Level0.solutions
  println(s"Level0 : ${sols.size} solutions :\n${Level0.solution}")

  object Level1 extends Level {
    val size = 2
    val pieces = List(2, 2, 2, 2, 2, 2, 2)
  }

  sols = Level1.solutions
  println(s"Level1 : ${sols.size} solutions :\n${Level1.solution}")

  object Level2 extends Level {
    val size = 3
    val pieces = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)
  }

  sols = Level2.solutions
  println(s"Level2 : ${sols.size} solutions :\n${Level2.solution}\n${Level2.hrSolution}")

  object Level3 extends Level {
    val size = 4
    val pieces = List(2, 4,
        2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 2, 4, 2, 2, 2, 4, 3, 2, 2,
        2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2,
        2, 2, 3, 2, 3, 2, 3, 2, 4, 2, 2, 3, 2, 3)
  }
        
  sols = Level3.solutions
  println(s"Level3 : ${sols.size} solutions :\n${Level3.solution}\n${Level3.hrSolution}")
  
}