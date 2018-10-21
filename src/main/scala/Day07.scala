object Day07 {
  def first(input: String): Int = {
    input.split("\n").count(tls)
  }

  def second(input: String): Int = {
    input.split("\n").count(ssl)
  }

  def tls(input: String): Boolean = {
    hasAbba(stripHypernet(input)) && !extractHypernet(input).exists(hasAbba)
  }

  def ssl(input: String): Boolean = {
    val babs = getBab(stripHypernet(input))
    if (babs.isEmpty)
      return false
    val hyper = extractHypernet(input)
    babs.exists(bab => hyper.exists(_.contains(bab)))
  }

  lazy private val regexp = "\\[(.+?)\\]"

  def extractHypernet(input: String): Seq[String] = {
    regexp.r.findAllMatchIn(input).map(_.group(1)).toSeq
  }

  def stripHypernet(input: String): String = {
    input.replaceAll(regexp, "-")
  }

  def hasAbba(input: String): Boolean = {
    0 to input.length - 4 exists (idx => {
      val trial = input.substring(idx, idx + 4)
      trial(1) == trial(2) && trial(0) == trial(3) && trial(0) != trial(1)
    })
  }

  def getBab(input: String): Seq[String] = {
    val tries = for (idx <- 0 to input.length - 3) yield input.substring(idx, idx + 3)
    val abas = tries.filter(trial => trial(0) == trial(2) && trial(0) != trial(1))
    abas.map(aba => s"${aba(1)}${aba(0)}${aba(1)}")
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day07.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """abba[mnop]qrst
        |abcd[bddb]xyyx
        |aaaa[qwer]tyui
        |ioxxoj[asdfgh]zxcvbn""".stripMargin, 2)

    println(first(lines))

    test(second,
      """aba[bab]xyz
        |xyx[xyx]xyx
        |aaa[kek]eke
        |zazbz[bzb]cdb""".stripMargin, 3)

    println(second(lines))
  }
}
