package cube

trait Solver extends CubeDef {
  val size: Int
  val pieces: List[Int]
  lazy val cube: Cube = Cube(size, Stream())

  def legalCubes(s: Int, c: Cube) =
    c.legalNeighbors(s).map { s =>
      Cube(size, s)
    }.toStream

  def recSolution(l: List[Int], cubes: Stream[Cube]): Stream[Cube] = l match {
    case Nil => cubes
    case p :: tail => {
      val res = for {
        c <- cubes
        i <- legalCubes(p, c)
      } yield i
      //      if (!res.isEmpty && res.head.vol.size > 55)
      //        println(s"${res.head.vol.size}(${res.head.vol.head}) : $l")
      res.foldLeft(Stream[Cube]())((a, px) => a match {
        case h #:: _ =>
          if (h.isDone)
            Stream(h)
          else
            recSolution(tail, px #:: a)
        case _ => recSolution(tail, px #:: a)
      })
    }
  }

  lazy val solutions: Stream[Cube] =
    recSolution(pieces, Stream(cube))

  lazy val solution: List[Pos] =
    solutions match {
      case h #:: _ => h.vol.toList
      case _ => List[Pos]()
    }

  lazy val hrSolution: List[Axis] =
    solution.zip(solution.tail).foldRight(List[Axis]())((p, a) => Cube(0, Stream(p._1, p._2)).lastAxis :: a)
}