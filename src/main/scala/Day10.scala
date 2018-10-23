object Day10 {
  def first(input: String): Int = ???

  def second(input: String): Int = ???

  lazy private val init = "value (\\d+) goes to bot (\\d+)".r
  def init(map: Map[Int, Bot], instructions: Seq[String]): Unit = {
    instructions.foreach { case init(v, bot) => addValue(map, bot.toInt, v.toInt) }
  }

  lazy private val move = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)".r
  def run(map: Map[Int, Bot], outputs: Map[Int, Int], instructions: Seq[String], stopExec: Map[Int, Bot] => Boolean = _ => true): Unit = {
    do {
      instructions.foreach {
        case move(from, lowType, lowVal, highType, highVal) =>
          val values = getValues(map, from.toInt)
          if (values.isDefined) {
            if (lowType == "bot")
              addValue(map, lowVal.toInt, values.get._1)
            else
              outputs(lowVal.toInt) = values.get._1
            if (highType == "bot")
              addValue(map, highVal.toInt, values.get._2)
            else
              outputs(highVal.toInt) = values.get._2
          }
      }
    } while (!stopExec(map))
  }

  def addValue(map: Map[Int, Bot], bot: Int, value: Int): Unit = {
    if (map.contains(bot)) {
      map(bot) += value
    } else {
      map(bot) = new Bot(value)
    }
  }

  def getValues(map: Map[Int, Bot], bot: Int): Option[(Int, Int)] = {
    val res = map(bot).values
    if (res.isDefined)
      map(bot) = new Bot()
    res
  }

  class Bot (val a: Option[Int] = None, val b: Option[Int] = None) {
    def this(value: Int) = this(Some(value))

    def +(v: Int): Bot = if (a.isDefined) new Bot(a, Some(v)) else new Bot(Some(v))

    lazy val values: Option[(Int, Int)] = {
      if (a.isEmpty || b.isEmpty)
        None
      else {
        val av = a.get
        val bv = b.get
        if (av < bv)
          Some(av, bv)
        else
          Some(bv, av)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day10.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)
    test(first, lines, true)
  }
}


//value 5 goes to bot 2
//bot 2 gives low to bot 1 and high to bot 0
//value 3 goes to bot 1
//bot 1 gives low to output 1 and high to bot 0
//bot 0 gives low to output 2 and high to output 0
//value 2 goes to bot 2
//
//    Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
//    Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
//    Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
//    Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.
//
//In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2
// contains a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips
// with value-2 microchips.
//
//Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with
// value-17 microchips?
//
//Your puzzle answer was 27.
//--- Part Two ---
//
//What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
//
//Your puzzle answer was 13727.
