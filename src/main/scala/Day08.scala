object Day08 {
  def first(input: String): Int = {
    val screen = new Screen()
    run(screen, input.split("\n"))
    screen.count()
  }

  def second(input: String): String = {
    val screen = new Screen()
    run(screen, input.split("\n"))
    screen.toString
  }

  private lazy val reRect = "rect (\\d+)x(\\d+)".r
  private lazy val reRotateRow = "rotate row y=(\\d+) by (\\d+)".r
  private lazy val reRotateColumn = "rotate column x=(\\d+) by (\\d+)".r

  def run(screen: Screen, input: Seq[String]): Unit = {
    input foreach {
      case reRect(a, b) => screen.rect(a.toInt, b.toInt)
      case reRotateRow(a, b) => screen.rotateRow(a.toInt, b.toInt)
      case reRotateColumn(a, b) => screen.rotateColumn(a.toInt, b.toInt)
    }
  }

  class Screen(w: Int = 50, h: Int = 6) {
    private val screen = Array.ofDim[Boolean](h, w)

    def rect(w: Int, h: Int): Unit = {
      for (wi <- 0 until w; hi <- 0 until h) {
        screen(hi)(wi) = true
      }
    }

    def rotateRow(y: Int, amount: Int): Unit = {
      1 to amount foreach(_ => rotateRow(y))
    }

    def rotateRow(y: Int): Unit = {
      val sv = screen(y).last
      for (i <- screen(y).length -1 to 1 by -1)
        screen(y)(i) = screen(y)(i - 1)
      screen(y)(0) = sv
    }

    def rotateColumn(x: Int, amount: Int): Unit = {
      1 to amount foreach(_ => rotateColumn(x))
    }

    def rotateColumn(x: Int): Unit = {
      val sv = screen.last(x)
      for (i <- screen.length -1 to 1 by -1)
        screen(i)(x) = screen(i - 1)(x)
      screen(0)(x) = sv
    }

    def count(): Int = {
      screen.map(_.count(identity)).sum
    }

    override def toString: String = {
      screen.map(_.map(b => if (b) "#" else " ").mkString).mkString("\n")
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day08.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
