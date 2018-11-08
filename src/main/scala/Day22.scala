object Day22 {
  def first(input: String): Int = {
    val nodes = input.split("\n").map(parse).filter(_.isDefined).map(_.get)

    nodes.combinations(2).flatMap(_.permutations).count(c => viable(c(0), c(1)))
  }

  def second(input: String): Int = ???

  case class Node(name: String, size: Int, used: Int)

  def viable(from: Node, to: Node): Boolean = from.used > 0 && to.used + from.used <= to.size

  private val reg = "(.+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T.*".r
  def parse(in: String): Option[Node] =
    in match {
      case reg(name, s, u, a) =>
        val size = s.toInt
        val used = u.toInt
        val avail = a.toInt
        assert(used + avail == size)
        Some(Node(name, size, used))
      case _ => None
    }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day22.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))
  }
}
