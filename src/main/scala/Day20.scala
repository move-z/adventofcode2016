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

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    println(first(lines))

    println(second(lines))
  }
}

//You'd like to set up a small hidden computer here so you can use it to get back into the network later. However, the
// corporate firewall only allows communication with certain external IP addresses.
//
//You've retrieved the list of blocked IPs from the firewall, but the list seems to be messy and poorly maintained, and
// it's not clear which IPs are allowed. Also, rather than being written in dot-decimal notation, they are written as
// plain 32-bit integers, which can have any value from 0 through 4294967295, inclusive.
//
//For example, suppose only the values 0 through 9 were valid, and that you retrieved the following blacklist:
//
//5-8
//0-2
//4-7
//
//The blacklist specifies ranges of IPs (inclusive of both the start and end value) that are not allowed. Then, the only
// IPs that this firewall allows are 3 and 9, since those are the only numbers not in any range.
//
//Given the list of blocked IPs you retrieved from the firewall (your puzzle input), what is the lowest-valued IP that
// is not blocked?
//
//Your puzzle answer was 14975795.
//--- Part Two ---
//
//How many IPs are allowed by the blacklist?
//
//Your puzzle answer was 101.
