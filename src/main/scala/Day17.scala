import java.security.MessageDigest

import scala.annotation.tailrec

object Day17 {
  def first(input: String): String = {
    val floor = Floor(input)

    @tailrec def travel(from: Seq[floor.Room]): floor.Room = {
      val o = from.find(_.vault)
      if (o.isDefined)
        o.get
      else
        travel(from.flatMap(_.next()))
    }

    travel(Seq(floor.start)).path
  }

  def second(input: String): Int = {
    val floor = Floor(input)

    def travel(from: Seq[floor.Room]): Seq[floor.Room] = {
      val (end, oth) = from.flatMap(_.next()).partition(_.vault)

      if (oth.nonEmpty) {
        val next = travel(oth)
        if (next.nonEmpty)
          next
        else
          end
      } else {
        end
      }
    }

    travel(Seq(floor.start)).head.path.length
  }

  lazy private val md5 = MessageDigest.getInstance("MD5")

  case class Floor(key: String) {
    val start: Room = new Room(0, 0)

    class Room(x: Int, y: Int, val path: String = "") {
      def next(): Seq[Room] = {
        var r: Seq[Room] = Nil

        val dig = digest.slice(0, 4)

        if (x > 0 && dig(0) > 'a')
          r = new Room(x - 1, y, path + "U") +: r
        if (x < 3 && dig(1) > 'a')
          r = new Room(x + 1, y, path + "D") +: r
        if (y > 0 && dig(2) > 'a')
          r = new Room(x, y - 1, path + "L") +: r
        if (y < 3 && dig(3) > 'a')
          r = new Room(x, y + 1, path + "R") +: r

        r
      }

      private lazy val digest = md5.digest((key + path).getBytes).map("%02x".format(_)).mkString

      def vault: Boolean = x == 3 && y == 3
    }
  }

  def main(args: Array[String]): Unit = {
    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, "ihgpwlah", "DDRRRD")
    test(first, "kglvqrro", "DDUDRLRRUDRD")
    test(first, "ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR")

    println(first("mmsxrhfx"))
    test(first, "mmsxrhfx", "RLDUDRDDRR")

    test(second, "ihgpwlah", 370)
    test(second, "kglvqrro", 492)
    test(second, "ulqzkmiv", 830)

    println(second("mmsxrhfx"))
    test(second, "mmsxrhfx", 590)
  }
}
