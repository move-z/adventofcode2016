import java.lang.Math.abs

import scala.annotation.tailrec

object Day22 {
  def first(input: String): Int = {
    val nodes = input.split("\n").map(parse).filter(_.isDefined).map(_.get)

    nodes.combinations(2).flatMap(_.permutations).count(c => viable(c(0), c(1)))
  }

  def second(input: String): Int = {
    val nodes = input.split("\n").map(parse).filter(_.isDefined).map(_.get)

    val empty = nodes.find(_.used == 0).get
    val (top, stepsToTop) = rise(empty, nodes)

    val width = nodes.maxBy(_.x).x
    val stepsToData = width - top.y - 1

    val loopSize = width * 2
    val stepsToTgt = (loopSize - 1) * (width - 1) + 1

    stepsToTop + stepsToData + stepsToTgt
  }

  case class Node(x: Int, y: Int, size: Int, used: Int)

  def viable(from: Node, to: Node): Boolean = from.used > 0 && to.used + from.used <= to.size

  def rise(from: Node, nodes: Seq[Node]): (Node, Int) = {
    @tailrec def _r(f: Node, acc: Int = 0): (Node, Int) = {
      if (f.y == 0)
        (f, acc)
      else {
        val next = find(f.size, f.x, nodes.filter(_.y == f.y - 1))
        _r(next, acc + 1 + abs(next.x - f.x))
      }
    }
    _r(from)
  }

  def find(space: Int, fromX: Int, nodes: Seq[Node]): Node = {
    def fit(n: Node) = space <= n.size
    if (fit(nodes(fromX)))
      nodes(fromX)
    else
      nodes.filter(fit).minBy(s => abs(s.x - fromX))
  }

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
