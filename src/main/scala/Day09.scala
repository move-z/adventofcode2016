object Day09 {
  def first(input: String): Int = size(input.stripLineEnd)

  def second(input: String): Long = size2(input.stripLineEnd)

  lazy private val reg = "(.*?)?(\\((\\d+)x(\\d+)\\)).*".r

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

  def size2(input: String): Long = {
    input match {
      case reg(start, r, repSizeS, nS) =>
        val repSize = repSizeS.toInt
        val n = nS.toInt
        val startIdx = start.length + r.length
        val endIdx = startIdx + repSize
        val rep = input.substring(startIdx, endIdx) * n
        val remain = rep + input.substring(endIdx)
        start.length + size2(remain)
      case _ => input.length
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

    test(second, lines, 11797310782L)
    println(second(lines))
  }
}


//The format compresses a sequence of characters. Whitespace is ignored. To indicate that some sequence should be
// repeated, a marker is added to the file, like (10x2). To decompress this marker, take the subsequent 10 characters
// and repeat them 2 times. Then, continue reading the file after the repeated data. The marker itself is not included
// in the decompressed output.
//
//If parentheses or other characters appear within the data referenced by a marker, that's okay - treat it like normal
// data, not a marker, and then resume looking for markers after the decompressed section.
//
//For example:
//
//    ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
//    A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
//    (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
//    A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
//    (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of
// another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
//    X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from
// the (8x2) marker (the (3x3)ABC) is skipped and not processed further.
//
//What is the decompressed length of the file (your puzzle input)? Don't count whitespace.
//
//Your puzzle answer was 152851.
//--- Part Two ---
//
//Apparently, the file actually uses version two of the format.
//
//In version two, the only difference is that markers within decompressed data are decompressed. This, the documentation
// explains, provides much more substantial compression capabilities, allowing many-gigabyte files to be stored in only
// a few kilobytes.
//
//For example:
//
//    (3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
//    X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further
// decompressed, thus triggering the (3x3) marker twice for a total of six ABC sequences.
//    (27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
//    (25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
//
//Unfortunately, the computer you brought probably doesn't have enough memory to actually decompress the file; you'll
// have to come up with another way to get its decompressed length.
//
//What is the decompressed length of the file using this improved format?
//
//Your puzzle answer was 11797310782.
