object Day09 {
  def first(input: String): Int = size(input.stripLineEnd)

  def second(input: String): Long = size2(input.stripLineEnd)

  lazy private val reg = "(.*?)(\\((\\d+)x(\\d+)\\)).*".r

  def size(input: String): Int = {
    input match {
      case reg(start, r, repSizeS, nS) =>
        val repSize = repSizeS.toInt
        val n = nS.toInt
        val remain = input.substring(start.length + r.length + repSize)
        start.length + repSize * n + size(remain)
      case _ => input.length
    }
  }

  def size2(input: String, currSize: Long = 0): Long = {
    input match {
      case reg(start, r, repSizeS, nS) =>
        val repSize = repSizeS.toInt
        val n = nS.toInt
        val startIdx = start.length + r.length
        val endIdx = startIdx + repSize
        val repLength = size2(input.substring(startIdx, endIdx)) * n
        val remain = input.substring(endIdx)
        size2(remain, start.length + repLength + currSize)
      case _ => input.length + currSize
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day09.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, "ADVENT", 6)
    test(first, "A(1x5)BC", 7)
    test(first, "(3x3)XYZ", 9)
    test(first, "A(2x2)BCD(2x2)EFG", 11)
    test(first, "(6x1)(1x3)A", 6)
    test(first, "X(8x2)(3x3)ABCY", 18)

    println(first(lines))

    test(second, "(3x3)XYZ", "XYZXYZXYZ".length)
    test(second, "X(8x2)(3x3)ABCY", "XABCABCABCABCABCABCY".length)
    test(second, "(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920)
    test(second, "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445)

    println(second(lines))
  }
}
