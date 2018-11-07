object Day21 {
  def first(input: String): String = {
    val arr = "abcdefgh".toCharArray
    input.split("\n").foreach(apply(arr, _))
    arr.mkString
  }

  def second(input: String): String = ???

  private val swapP = "swap position (\\d+) with position (\\d+)".r
  private val swapL = "swap letter (.) with letter (.)".r
  private val rotateLeft = "rotate left (\\d+) steps?".r
  private val rotateRight = "rotate right (\\d+) steps?".r
  private val rotateP = "rotate based on position of letter (.)".r
  private val reverse = "reverse positions (\\d+) through (\\d+)".r
  private val move = "move position (\\d+) to position (\\d+)".r

  def apply(arr: Array[Char], instruction: String): Unit = {
    def _swapP(a: Int, b: Int): Unit = {
      val c = arr(a)
      arr(a) = arr(b)
      arr(b) = c
    }
    def _swapL(a: Char, b: Char): Unit = {
      val ai = arr.indexOf(a)
      val bi = arr.indexOf(b)
      val c = arr(ai)
      arr(ai) = arr(bi)
      arr(bi) = c
    }
    def _rotateLeft(n: Int): Unit = {
      val _n = if (n > arr.length) n % arr.length else n
      val cpy = arr.clone()
      val split = cpy.splitAt(_n)
      split._2.indices.foreach(i => arr(i) = split._2(i))
      split._1.indices.foreach(i => arr(i + arr.length - _n) = split._1(i))
    }
    def _rotateRight(n: Int): Unit = {
      val _n = if (n > arr.length) n % arr.length else n
      val cpy = arr.clone()
      val split = cpy.splitAt(arr.length - _n)
      split._1.indices.foreach(i => arr(i + _n) = split._1(i))
      split._2.indices.foreach(i => arr(i) = split._2(i))
    }
    def _rotateP(l: Char): Unit = {
      val idx = arr.indexOf(l) + 1
      val n = if (idx > 4) idx + 1 else idx
      _rotateRight(n)
    }
    def _reverse(from: Int, to: Int): Unit = {
      val r = (to to from by -1).map(arr(_))
      r.indices.foreach(i => arr(i + from) = r(i))
    }
    def _move(from: Int, to: Int): Unit = {
      val c = arr(from)
      if (from < to)
        (from until to).foreach(i => arr(i) = arr(i + 1))
      else
        (from until to by -1).foreach(i => arr(i) = arr(i - 1))
      arr(to) = c
    }

    instruction match {
      case swapP(a, b) => _swapP(a.toInt, b.toInt)
      case swapL(a, b) => _swapL(a.head, b.head)
      case rotateLeft(n) => _rotateLeft(n.toInt)
      case rotateRight(n) => _rotateRight(n.toInt)
      case rotateP(l) => _rotateP(l.head)
      case reverse(from, to) => _reverse(from.toInt, to.toInt)
      case move(from, to) => _move(from.toInt, to.toInt)
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day21.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))

//    println(second(lines))
  }
}

//You scrambled the password correctly, but you discover that you can't actually modify the password file on the system.
// You'll need to un-scramble one of the existing passwords by reversing the scrambling process.
//
//What is the un-scrambled version of the scrambled password fbgdceah?
