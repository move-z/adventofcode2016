import scala.collection.mutable

object Day10 {
  def first(input: String): Int = {
    val instructions = input.split("\n")
    val bots = mutable.Map[Int, Bot]()
    val outputs = mutable.Map[Int, Int]()

    def find(): Option[Int] = {
      bots.mapValues(_.values).filter(_._2.isDefined).find(b => b._2.get._1 == 17 && b._2.get._2 == 61).map(_._1)
    }

    init(bots, instructions)
    run(bots, outputs, instructions, _ => find().isDefined)
    find().get
  }

  def second(input: String): Int = ???

  lazy private val init = "value (\\d+) goes to bot (\\d+)".r

  def init(map: mutable.Map[Int, Bot], instructions: Seq[String]): Unit = {
    instructions.foreach {
      case init(v, bot) => addValue(map, bot.toInt, v.toInt)
      case _ =>
    }
  }

  lazy private val move = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)".r

  def run(map: mutable.Map[Int, Bot], outputs: mutable.Map[Int, Int], instructions: Seq[String], stopExec: mutable.Map[Int, Bot] => Boolean = _ => true): Unit = {
    var running = true
    do {
      running = false
      instructions.foreach(running ||= exec(map, outputs, _))
    } while (!stopExec(map) && running)
  }

  def exec(map: mutable.Map[Int, Bot], outputs: mutable.Map[Int, Int], instruction: String): Boolean = {
    instruction match {
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
          true
        } else {
          false
        }
      case _ => false
    }
  }

  def addValue(map: mutable.Map[Int, Bot], bot: Int, value: Int): Unit = {
    if (map.contains(bot)) {
      map(bot) += value
    } else {
      map(bot) = new Bot(value)
    }
  }

  def getValues(map: mutable.Map[Int, Bot], bot: Int): Option[(Int, Int)] = {
    if (!map.contains(bot))
      None
    else {
      val res = map(bot).values
      if (res.isDefined)
        map(bot) = new Bot()
      res
    }
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

    val instructions =
      """value 5 goes to bot 2
        |bot 2 gives low to bot 1 and high to bot 0
        |value 3 goes to bot 1
        |bot 1 gives low to output 1 and high to bot 0
        |bot 0 gives low to output 2 and high to output 0
        |value 2 goes to bot 2""".stripMargin.split("\n")
    val bots = mutable.Map[Int, Bot]()
    val outputs = mutable.Map[Int, Int]()
    init(bots, instructions)
    run(bots, outputs, instructions, _ => outputs.contains(0) && outputs.contains(1) && outputs.contains(2))

    println(first(lines))
    test(first, lines, 27)

//    test(first, , 27)
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
