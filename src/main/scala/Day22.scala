import scala.annotation.tailrec
import scala.collection.mutable

object Day22 {
  def first(input: String): Int = {
    val nodes = input.split("\n").map(parse).filter(_.isDefined).map(_.get)

    def viable(from: Node, to: Node): Boolean = from.used > 0 && to.used + from.used <= to.size

    nodes.combinations(2).flatMap(_.permutations).count(c => viable(c(0), c(1)))
  }

  def second(input: String): Int = {
    val nodes = input.split("\n").map(parse).filter(_.isDefined).map(_.get)
    val cluster = Cluster(nodes.map(n => (n.x, n.y) -> n).toMap)

    val seen = mutable.Set[Cluster]()

    @tailrec def find(in: Seq[Cluster], steps: Int = 0): Int = {
      println(steps)
      println(in.length)
      if (in.exists(_.tgt == (0, 0)))
        steps
      else
        find(in.flatMap(_.next).filter(seen.add), steps + 1)
    }

    find(Seq(cluster))
  }

  case class Cluster(nodes: Map[(Int, Int), Node], tgt: (Int, Int) = (34, 0)) {
    val empty: Node = nodes.values.find(_.used == 0).get

    def next: Seq[Cluster] = {
      def opt(bool: Boolean) = if (bool) Some(true) else None

      def fits(x: Int, y: Int) = {
        val n = nodes(x, y)
        if (empty.size < n.used) None else Some(n)
      }

      def move(from: Node, to: Node): Cluster = {
        val f = Node(from.x, from.y, from.size, 0)
        val t = Node(to.x, to.y, to.size, from.used)
        val newTgt = if (from.x == tgt._1 && from.y == tgt._2) (to.x, to.y) else tgt

        Cluster(nodes + ((f.x, f.y) -> f) + ((t.x, t.y) -> t), newTgt)
      }

      val r =
      opt(empty.x > 0).flatMap(_ => fits(empty.x - 1, empty.y)).map(move(_, empty)) +:
      opt(empty.x < 34).flatMap(_ => fits(empty.x + 1, empty.y)).map(move(_, empty)) +:
      opt(empty.y > 0).flatMap(_ => fits(empty.x, empty.y - 1)).map(move(_, empty)) +:
//      opt(empty.x < 34).flatMap(_ => fits(empty.x, empty.y + 1)).map(move(_, empty)) +:
      Nil

      r.filter(_.isDefined).map(_.get)
    }

  }

  case class Node(x: Int, y: Int, size: Int, used: Int)

  private val reg = "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T.*".r
  def parse(in: String): Option[Node] =
    in match {
      case reg(x, y, s, u, a) =>
        val size = s.toInt
        val used = u.toInt
        val avail = a.toInt
        assert(used + avail == size)
        Some(Node(x.toInt, y.toInt, size, used))
      case _ => None
    }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day22.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
