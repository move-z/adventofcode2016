object Day01 {
  def first(input: String): Boolean = ???

  def second(input: String): Boolean = ???


  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day01.txt")
    val lines = try source.mkString finally source.close()

    assert(first(lines))
  }
}


