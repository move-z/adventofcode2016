object Day06 {
  def first(input: String): String = {
    val strings = input.split("\n")
    reverseStrings(strings).map(mostUsedChar).mkString
  }

  def second(input: String): String = {
    val strings = input.split("\n")
    reverseStrings(strings).map(leastUsedChar).mkString
  }

  def reverseStrings(input: Seq[String]): Seq[Seq[Char]] = {
    for (idx <- input.head.indices) yield input.map(_(idx))
  }

  def mostUsedChar(input: Seq[Char]): Char = {
    input.groupBy(identity).mapValues(_.length).maxBy(_._2)._1
  }

  def leastUsedChar(input: Seq[Char]): Char = {
    input.groupBy(identity).mapValues(_.length).minBy(_._2)._1
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day06.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, """eedadn
                  |drvtee
                  |eandsr
                  |raavrd
                  |atevrs
                  |tsrnev
                  |sdttsa
                  |rasrtv
                  |nssdts
                  |ntnada
                  |svetve
                  |tesnvt
                  |vntsnd
                  |vrdear
                  |dvrsen
                  |enarar""".stripMargin, "easter")

    println(first(lines))

    println(second(lines))
  }
}
