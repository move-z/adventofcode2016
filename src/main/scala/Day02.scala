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

  def second(input: String): String = ???

  def advance(input: String, pad: Pad): Pad = {
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
    test(first, lines, "48584")

    test(second,
      """ULL
        |RRDDD
        |LURDL
        |UUUUD""".stripMargin, "5DB3")
    test(second, lines, "563B6")
  }
}


//You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy
// conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the
// keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of
// bathroom-keypad-design meetings:
//
//    1
//  2 3 4
//5 6 7 8 9
//  A B C
//    D
//
//You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very
// different:
//
//    You start at "5" and don't move at all (up and left are both edges), ending at 5.
//    Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
//    Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
//    Finally, after five more moves, you end at 3.
//
//So, given the actual keypad layout, the code would be 5DB3.
//
//Using the same instructions in your puzzle input, what is the correct bathroom code?
//
//Your puzzle answer was 563B6.
