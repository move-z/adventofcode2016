object Day18 {
  def first(input: String): Int = {
    val start = input.stripLineEnd.map(c => if (c == '.') false else true)
    val floors = (2 to 40).foldLeft[Seq[Seq[Boolean]]](Seq(start))((e, _) => nextFloor(e.head) +: e)
    floors.map(_.count(!_)).sum
  }

  def second(input: String): Int = {
    val start = input.stripLineEnd.map(c => if (c == '.') false else true)
    val floors = (2 to 400000).foldLeft[Seq[Seq[Boolean]]](Seq(start))((e, _) => nextFloor(e.head) +: e)
    floors.map(_.count(!_)).sum
  }

  def nextFloor(current: Seq[Boolean]): Seq[Boolean] =
    current.indices.map(i => safe(i, current))

  def safe(idx: Int, upperFloor: Seq[Boolean]): Boolean = {
    val l = if (idx > 0) upperFloor(idx - 1) else false
    val r = if (idx < upperFloor.length - 1) upperFloor(idx + 1) else false
    l ^ r
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day18.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
