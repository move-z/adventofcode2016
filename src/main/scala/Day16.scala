import scala.annotation.tailrec

object Day16 {
  def first(input: String): String = {
    val d = Disk.from(input)
    val s = (0 until 272).map(d.elementAt)
    chkSum(s).map(e => if (e) '1' else '0').mkString
  }

  def second(input: String): String = {
    val d = Disk.from(input)
    val s = (0 until 35651584).map(d.elementAt)
    chkSum(s).map(e => if (e) '1' else '0').mkString
  }

  // filling follows this pattern:
  // a <conn> b <conn> a <conn> b ...
  // where conn follows this series:
  // 01010... for even positions (zero based)
  // the series itself recursively for odd positions
  class Disk(seed: Seq[Boolean]) {
    lazy private val step = seed.length + 1
    lazy private val antiSeed = seed.map(c => if (c) false else true).reverse

    def elementAt(idx: Int): Boolean = {
      val pos = idx % (step * 2)
      if (pos == step - 1 || pos == (step * 2) - 1)
        connAt(idx / step)
      else if (pos < step - 1)
        seed(pos)
      else
        antiSeed(pos - step)
    }

    @tailrec private def connAt(idx: Int): Boolean = {
      if (idx % 2 == 0)
        if ((idx / 2) % 2 == 0) false else true
      else
        connAt((idx - 1) / 2)
    }
  }

  object Disk {
    def from(s: String): Disk = new Disk(s.map(c => if (c == '0') false else true))
  }

  @tailrec def chkSum(input: Seq[Boolean]): Seq[Boolean] = {
    val chk = input.grouped(2).map(s => !(s(0) ^ s(1))).toSeq
    if (chk.length % 2 == 0)
      chkSum(chk)
    else
      chk
  }

  def main(args: Array[String]): Unit = {
    println(first("11110010111001001"))
    println(second("11110010111001001"))
  }
}
