package cube

trait CubeDef {
  case class Pos(x: Int, y: Int, z: Int) {
    def dx(n: Int) = copy(x = x + n)
    def dy(n: Int) = copy(y = y + n)
    def dz(n: Int) = copy(z = z + n)
  }

  val zero = new Pos(0, 0, 0)

  sealed abstract class Axis
  case object XAxis extends Axis
  case object YAxis extends Axis
  case object ZAxis extends Axis

  def getMaxDistance(l: List[Int]): Int = l match {
    case Nil => 0
    case _ => l.min.abs + l.max.abs + 1
  }

  case class Cube(size: Int, vol: Stream[Pos]) {

    def lastAxis: Axis = vol match {
      case b #:: a #:: _ => {
        if ((b.x.abs - a.x.abs).abs > 0)
          XAxis
        else if ((b.y.abs - a.y.abs).abs > 0)
          YAxis
        else
          ZAxis
      }
      case _ => ZAxis
    }

    def placePiece(s: Int)(f: Pos => Pos): Stream[Pos] = {
      def recPlacePiece(s: Int, r: Stream[Pos]): Stream[Pos] = {
        if (s == 0)
          r
        else if (!r.isEmpty)
          recPlacePiece(s - 1, f(r.head) #:: r)
        else
          throw new Exception("placeFirstPiece is shit ?")
      }
      if (s == 0)
        vol
      else if (vol.isEmpty)
        recPlacePiece(s - 1, Stream(zero))
      else
        recPlacePiece(s - 1, vol)
    }

    // Force the 2 first pieces
    // works with 0, 1, 2 or more pieces
    def placeTwoFirstPieces(pieces: List[Int]): Cube = pieces match {
      case a :: b :: _ =>
        Cube(size, Cube(size, placePiece(a)(_.dx(1))).placePiece(b)(_.dy(1)))
      case a :: Nil => Cube(size, placePiece(a)(_.dx(1)))
      case Nil => this
    }

    def drop(n: Int): Cube =
      copy(vol = vol.drop(n))

    def neighbors(s: Int): List[Stream[Pos]] = lastAxis match {
      case XAxis =>
        List(placePiece(s)(_.dy(1)),
          placePiece(s)(_.dy(-1)),
          placePiece(s)(_.dz(1)),
          placePiece(s)(_.dz(-1)))
      case YAxis =>
        List(placePiece(s)(_.dx(1)),
          placePiece(s)(_.dx(-1)),
          placePiece(s)(_.dz(1)),
          placePiece(s)(_.dz(-1)))
      case ZAxis =>
        List(placePiece(s)(_.dx(1)),
          placePiece(s)(_.dx(-1)),
          placePiece(s)(_.dy(1)),
          placePiece(s)(_.dy(-1)))
    }

    def legalNeighbors(s: Int): List[Stream[Pos]] =
      neighbors(s).filter(p => isValid(p))

    def isValid(v: Stream[Pos]) =
      (v.size == v.distinct.size) &&
        (getMaxDistance(v.foldLeft(List[Int]())((l, p) => p.x :: l)) <= size) &&
        (getMaxDistance(v.foldLeft(List[Int]())((l, p) => p.y :: l)) <= size) &&
        (getMaxDistance(v.foldLeft(List[Int]())((l, p) => p.z :: l)) <= size)

    def isValid: Boolean = isValid(vol)

    def isDone: Boolean = isValid && vol.size == Math.pow(size, 3)
  }
}