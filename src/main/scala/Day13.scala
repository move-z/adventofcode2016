import scala.collection.mutable

object Day13 {
  def first(input: Int, x: Int = 31, y: Int = 39): Int = {
    var count = 0
    val seen = mutable.Set[Office]()
    var current = Office(input) :: Nil

    while (!current.exists(o => o.curx == x && o.cury == y)) {
      seen ++= current
      count += 1
      current = current.flatMap(_.next()).distinct.filterNot(seen.contains)
    }

    count
  }

  def second(input: Int): Boolean = ???

  case class Office(seed: Int, curx: Int = 1, cury: Int = 1) {
    def next(): Seq[Office] = {
      var res: List[Office] = Nil
      if (curx > 0 && open(curx - 1, cury))
        res = Office(seed, curx - 1, cury) :: res
      if (open(curx + 1, cury))
        res = Office(seed, curx + 1, cury) :: res
      if (cury > 0 && open(curx, cury - 1))
        res = Office(seed, curx, cury - 1) :: res
      if (open(curx, cury + 1))
        res = Office(seed, curx, cury + 1) :: res
      res
    }

    private def open(x: Int, y: Int): Boolean = {
      val num = seed + x * x + 3 * x + 2 * x * y + y + y * y
      val ones = num.toBinaryString.count(_ == '1')
      if (ones % 2 == 0) true else false
    }

  }

  def main(args: Array[String]): Unit = {
    assert(first(10, 7, 4) == 11)

    assert(first(1350) == 92)
  }
}


//How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?
//
//Your puzzle answer was 124.
