object Day04 {
  def first(input: String): Int = {
    input.split("\n").map(Room(_)).filter(_.valid).map(_.id).sum
  }

  def second(input: String): Int = ???

  case class Room(name: String, id: Int, checksum: String) {
    lazy val valid: Boolean = {
      calculateChecksum(name) == checksum
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
  }
}

//Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a
// checksum in square brackets.
//
//A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with
// ties broken by alphabetization. For example:
//
//    aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between
// x, y, and z, which are listed alphabetically.
//    a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five
// are listed alphabetically.
//    not-a-real-room-404[oarel] is a real room.
//    totally-real-room-200[decoy] is not.
//
//Of the real rooms from the list above, the sum of their sector IDs is 1514.
//
//What is the sum of the sector IDs of the real rooms?
//
//Your puzzle answer was 361724.
//--- Part Two ---
//
//With all the decoy data out of the way, it's time to decrypt this list and get moving.
//
//The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right
// software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.
//
//To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector
// ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.
//
//For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
//
//What is the sector ID of the room where North Pole objects are stored?
//
//Your puzzle answer was 482.
