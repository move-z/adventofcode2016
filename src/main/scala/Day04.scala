object Day04 {
  def first(input: String): Int = {
    input.split("\n").map(Room(_)).filter(_.valid).map(_.id).sum
  }

  def second(input: String): Unit = {
    input.split("\n").map(Room(_)).filter(_.valid).foreach(r => {
      if (r.decrypted.contains("north"))
        println(s"%d - %s".format(r.id, r.decrypted))
    })
  }

  case class Room(name: String, id: Int, checksum: String) {
    lazy val valid: Boolean = {
      calculateChecksum(name) == checksum
    }

    lazy val decrypted: String = {
      name.map(c => {
        if (c < 'a' || c > 'z')
          ' '
        else {
          val base = c - 'a'
          val rot = (base + id) % 26
          (rot + 'a').toChar
        }
      }).mkString
    }
  }

  object Room {
    def apply(room: String): Room = {
      val reg = "(.+)-(\\d+)\\[(.+)\\]".r
      val reg(name, id, chk) = room
      new Room(name, id.toInt, chk)
    }
  }

  def calculateChecksum(input: String): String = {
    val clean = input.replaceAll("[^a-z]", "")
    val charOccurr = clean.groupBy(identity).mapValues(_.length)
    val charByOccurr = charOccurr.groupBy(_._2).mapValues(e => e.keys.toSeq.sorted.mkString)
    charByOccurr.toSeq.sortBy(_._1).reverse.map(_._2).mkString.substring(0, 5)
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day04.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """aaaaa-bbb-z-y-x-123[abxyz]
        |a-b-c-d-e-f-g-h-987[abcde]
        |not-a-real-room-404[oarel]
        |totally-real-room-200[decoy]""".stripMargin, 1514)

    println(first(lines))

    assert(Room("qzmt-zixmtkozy-ivhz", 343, "").decrypted == "very encrypted name")

    second(lines)
  }
}
