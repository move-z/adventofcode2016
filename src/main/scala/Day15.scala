import scala.annotation.tailrec

object Day15 {
  def first(input: String): Int = {
    val disks = input.split("\n").map(parse)
    findSlot(disks)
  }

  def second(input: String): Int = {
    val disks = (input + "Disc #0 has 11 positions; at time=0, it is at position 0.").split("\n").map(parse)
    findSlot(disks)
  }

  private val reg = "Disc #\\d+ has (\\d+) positions; at time=0, it is at position (\\d+).".r
  def parse(line: String): Disk = {
    line match {
      case reg(d, t) => Disk(d.toInt, t.toInt)
    }
  }

  @tailrec def findSlot(disks: Seq[Disk], t: Int = 0): Int = {
    def open(disks: Seq[Disk], t: Int) = {
      disks.indices.forall(i => disks(i).posAt(t + i + 1) == 0)
    }

    if (open(disks, t))
      t
    else
      findSlot(disks, t + 1)
  }

  case class Disk(length: Int, posAtZero: Int) {
    def posAt(t: Int): Int = {
      (posAtZero + t) % length
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day15.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """Disc #1 has 5 positions; at time=0, it is at position 4.
        |Disc #2 has 2 positions; at time=0, it is at position 1.""".stripMargin, 5)

    println(first(lines))

    println(second(lines))
    test(second, lines, 2353212)
  }
}
