object Day03 {
  def first(input: String): Int = {
    input.split("\n").map(l => l.trim.split("\\s+")).count(arr => {
      val a = arr(0).toInt
      val b = arr(1).toInt
      val c = arr(2).toInt
      a < b + c && b < a + c && c < a + b
    })
  }

  def second(input: String): Int = {
    val in = input.split("\n").map(l => l.trim.split("\\s+"))
    val trasl = for (x <- in.indices by 3; y <- 0 to 2)
      yield for (adj <- 0 to 2)
        yield in(x + adj)(y).toInt
    trasl.count(arr => {
      val a = arr(0)
      val b = arr(1)
      val c = arr(2)
      a < b + c && b < a + c && c < a + b
    })
  }


  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day03.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
