import scala.annotation.tailrec

object Day02 {
  def first(input: String): String = {
    val pad = Array[Seq[Option[String]]](Array(Some("1"), Some("2"), Some("3")), Array(Some("4"), Some("5"), Some("6")), Array(Some("7"), Some("8"), Some("9")))
    val start = Pad(pad, 1, 1)
    var res = new StringBuilder
    input.split("\n").foldLeft(start)((p, line) => {
      val next = advance(line, p)
      res ++= next.element
      next
    })
    res.toString()
  }

  def second(input: String): String = {
    val pad = Array[Seq[Option[String]]](Array(None, None, Some("1"), None, None), Array(None, Some("2"), Some("3"), Some("4"), None), Array(Some("5"), Some("6"), Some("7"), Some("8"), Some("9")), Array(None, Some("A"), Some("B"), Some("C"), None), Array(None, None, Some("D"), None, None))
    val start = Pad(pad, 0, 2)
    var res = new StringBuilder
    input.split("\n").foldLeft(start)((p, line) => {
      val next = advance(line, p)
      res ++= next.element
      next
    })
    res.toString()
  }

  @tailrec def advance(input: String, pad: Pad): Pad = {
    if (input.length > 0) {
      input.head match {
        case 'U' => advance(input.tail, pad.up())
        case 'D' => advance(input.tail, pad.down())
        case 'L' => advance(input.tail, pad.left())
        case 'R' => advance(input.tail, pad.right())
      }
    } else {
      pad
    }
  }

  case class Pad(disposition: Seq[Seq[Option[String]]], startx: Int, starty: Int) {
    def up(): Pad = {
      if (elementAt(startx, starty - 1).nonEmpty) {
        Pad(disposition, startx, starty - 1)
      } else {
        this
      }
    }

    def down(): Pad = {
      if (elementAt(startx, starty + 1).nonEmpty) {
        Pad(disposition, startx, starty + 1)
      } else {
        this
      }
    }

    def left(): Pad = {
      if (elementAt(startx - 1, starty).nonEmpty) {
        Pad(disposition, startx - 1, starty)
      } else {
        this
      }
    }

    def right(): Pad = {
      if (elementAt(startx + 1, starty).nonEmpty) {
        Pad(disposition, startx + 1, starty)
      } else {
        this
      }
    }

    def element: String = {
      disposition(starty)(startx).get
    }

    private def elementAt(x: Int, y: Int) = {
      if (y < 0 || y >= disposition.length || x < 0 || x >= disposition(y).length) {
        None
      } else {
        disposition(y)(x)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day02.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """ULL
        |RRDDD
        |LURDL
        |UUUUD""".stripMargin, "1985")
    println(first(lines))

    test(second,
      """ULL
        |RRDDD
        |LURDL
        |UUUUD""".stripMargin, "5DB3")
    println(second(lines))
  }
}
