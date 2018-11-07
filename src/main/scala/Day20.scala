object Day20 {
  def first(input: String): Long = {
    val intervals = getIntervals(input)
    if (intervals.head._1 > 0) {
      0
    } else {
      intervals.head._2 + 1
    }
  }

  def second(input: String): Int = {
    val intervals = getIntervals(input)
    intervals.sliding(2).map(s => s(1)._1 - s(0)._2 - 1).sum.toInt
  }

  type Interval = (Long, Long)

  private val r = "(\\d+)-(\\d+)".r
  def getIntervals(input: String): Seq[Interval] = {
    def mergeHead(s: Seq[Interval]): Seq[Interval] = {
      if (s.length == 1) {
        s
      } else {
        val h = s.head
        val t = s.tail
        if (h._2 >= t.head._1 - 1) {
          val e = if (h._2 > t.head._2) h._2 else t.head._2
          mergeHead((s.head._1, e) +: t.tail)
        } else {
          h +: mergeHead(t)
        }
      }
    }

    val intervals = input.split("\n").map { case r(f, t) => (f.toLong, t.toLong) } .sorted
    mergeHead(intervals)
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day20.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
