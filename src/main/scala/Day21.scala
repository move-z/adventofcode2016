object Day21 {
  def first(input: String): String = {
    val arr = "abcdefgh".toCharArray
    input.split("\n").foreach(apply(new Buffer(arr), _))
    arr.mkString
  }

  def second(input: String): String = {
    val arr = "fbgdceah".toCharArray
    input.split("\n").reverseIterator.foreach(unapply(new Buffer(arr), _))
    arr.mkString
  }

  private val swapP = "swap position (\\d+) with position (\\d+)".r
  private val swapL = "swap letter (.) with letter (.)".r
  private val rotateLeft = "rotate left (\\d+) steps?".r
  private val rotateRight = "rotate right (\\d+) steps?".r
  private val rotateP = "rotate based on position of letter (.)".r
  private val reverse = "reverse positions (\\d+) through (\\d+)".r
  private val move = "move position (\\d+) to position (\\d+)".r

  class Buffer(arr: Array[Char]) {
    def swapP(a: Int, b: Int): Unit = {
      val c = arr(a)
      arr(a) = arr(b)
      arr(b) = c
    }
    def swapL(a: Char, b: Char): Unit = {
      val ai = arr.indexOf(a)
      val bi = arr.indexOf(b)
      val c = arr(ai)
      arr(ai) = arr(bi)
      arr(bi) = c
    }
    def rotateLeft(n: Int): Unit = {
      val _n = if (n > arr.length) n % arr.length else n
      val cpy = arr.clone()
      val split = cpy.splitAt(_n)
      split._2.indices.foreach(i => arr(i) = split._2(i))
      split._1.indices.foreach(i => arr(i + arr.length - _n) = split._1(i))
    }
    def rotateRight(n: Int): Unit = {
      val _n = if (n > arr.length) n % arr.length else n
      val cpy = arr.clone()
      val split = cpy.splitAt(arr.length - _n)
      split._1.indices.foreach(i => arr(i + _n) = split._1(i))
      split._2.indices.foreach(i => arr(i) = split._2(i))
    }
    def rotateP(l: Char): Unit = {
      val n = arr.indexOf(l) + 1
      val idx = if (n > 4) n + 1 else n
      rotateRight(idx)
    }
    def reverseRotateP(l: Char): Unit = {
      val idx = arr.indexOf(l) match {
        case 0 => 1
        case 1 => 1
        case 2 => 6
        case 3 => 2
        case 4 => 7
        case 5 => 3
        case 6 => 0
        case 7 => 4
      }
      rotateLeft(idx)
    }
    def reverse(from: Int, to: Int): Unit = {
      val r = (to to from by -1).map(arr(_))
      r.indices.foreach(i => arr(i + from) = r(i))
    }
    def move(from: Int, to: Int): Unit = {
      val c = arr(from)
      if (from < to)
        (from until to).foreach(i => arr(i) = arr(i + 1))
      else
        (from until to by -1).foreach(i => arr(i) = arr(i - 1))
      arr(to) = c
    }
  }

  def apply(buf: Buffer, instruction: String): Unit = {
    instruction match {
      case swapP(a, b) => buf.swapP(a.toInt, b.toInt)
      case swapL(a, b) => buf.swapL(a.head, b.head)
      case rotateLeft(n) => buf.rotateLeft(n.toInt)
      case rotateRight(n) => buf.rotateRight(n.toInt)
      case rotateP(l) => buf.rotateP(l.head)
      case reverse(from, to) => buf.reverse(from.toInt, to.toInt)
      case move(from, to) => buf.move(from.toInt, to.toInt)
    }
  }

  def unapply(buf: Buffer, instruction: String): Unit = {
    instruction match {
      case swapP(a, b) => buf.swapP(a.toInt, b.toInt)
      case swapL(a, b) => buf.swapL(a.head, b.head)
      case rotateLeft(n) => buf.rotateRight(n.toInt)
      case rotateRight(n) => buf.rotateLeft(n.toInt)
      case rotateP(l) => buf.reverseRotateP(l.head)
      case reverse(from, to) => buf.reverse(from.toInt, to.toInt)
      case move(from, to) => buf.move(to.toInt, from.toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day21.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

    println(second(lines))
  }
}
