import scala.annotation.tailrec

object Day24 {
  def first(input: String): Int = {
    val (floor, pois) = parse(input.split("\n"))
    val startPos = pois.find(e => e._2 == '0').map(_._1).get

    F(floor).findPath(Seq((startPos, pois - startPos)), _._2.isEmpty)._2
  }

  def second(input: String): Int = {
    val (floor, pois) = parse(input.split("\n"))
    val startPos = pois.find(e => e._2 == '0').map(_._1).get

    F(floor).findPath(Seq((startPos, pois - startPos)), s => s._1 == startPos && s._2.isEmpty)._2
  }

  case class F(floor: Floor) {
    private def open(coord: Coord) = {
      val x = coord._1
      val y = coord._2
      y >= 0 && y < floor.length && x >= 0 && x < floor(y).length && floor(y)(x)
    }

    private def nextStates(curr: State) = {
      val c = curr._1
      val nextPos = Seq((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1)).filter(open)
      nextPos.map(c => (c, curr._2 - c))
    }

    type State = (Coord, Pois)
    private var cache = Set[State]()
    @tailrec final def findPath(curr: Seq[State], end: State => Boolean, dist: Int = 0): (Seq[State], Int) = {
      val tgt = curr.filter(end)
      if (tgt.nonEmpty)
        (tgt, dist)
      else {
        val next = curr.flatMap(nextStates).distinct.filterNot(cache.contains(_))
        cache ++= next
        findPath(next, end, dist + 1)
      }
    }

  }

  def parse(lines: Seq[String]): (Floor, Pois) = {
    var pois = Map[Coord, Char]()
    val floor = for (y <- lines.indices) yield {
      val line = lines(y)
      for (x <- line.indices) yield {
        line(x) match {
          case '#' => false
          case '.' => true
          case c =>
            pois += (x, y) -> c
            true
        }
      }
    }

    (floor, pois)
  }

  type Floor = Seq[Seq[Boolean]]
  type Coord = (Int, Int)
  type Pois = Map[Coord, Char]

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day24.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """###########
        |#0.1.....2#
        |#.#######.#
        |#4.......3#
        |###########""".stripMargin, 14)

    println(first(lines))

    println(second(lines))
  }
}


