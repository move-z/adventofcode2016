import scala.collection.mutable

object Day11 {
  def first(input: String): Int = {
    val floors = input.split("\n").map(l => Floor(parse(l)))
    val config = Config(0, floors)
    var configs = List(config)

    var count = 0
    do {
      count += 1
      configs = configs.flatMap(_.next())
      assert(configs.nonEmpty)
    } while (!configs.exists(_.end))

    count
  }

  def second(input: String): Int = {
    first("An elerium generator. An elerium-compatible microchip. A dilithium generator. A dilithium-compatible microchip." + input)
  }

  class Item(val element: String)

  case class Generator(override val element: String) extends Item(element)

  case class Chip(override val element: String) extends Item(element)

  private val generator = "\\s(\\w+) generator".r
  private val microchip = "\\s(\\w+)-compatible microchip".r

  def parse(line: String): Set[Item] = {
    val generators = generator.findAllMatchIn(line).map(e => Generator(e.group(1)))
    val microchips = microchip.findAllMatchIn(line).map(e => Chip(e.group(1)))
    (generators ++ microchips).toSet
  }

  case class Floor(items: Set[Item]) {
    lazy val movablePieces: Iterable[Set[Item]] = {
      def validPair(pair: Seq[Item]) = {
        pair(0).element == pair(1).element || pair(0).getClass == pair(1).getClass
      }
      val pairs = items.toSeq.combinations(2).filter(validPair).map(_.toSet)
      val singles = items.map(Set(_))
      (pairs ++ singles).toIterable
    }

    def moveItems(floor: Floor, pieces: Set[Item]): (Floor, Floor) = {
      val newFrom = items.diff(pieces)
      val newTo = floor.items ++ pieces
      (Floor(newFrom), Floor(newTo))
    }

    lazy val valid: Boolean = {
      val gens = items.filter(_.isInstanceOf[Generator]).asInstanceOf[Set[Generator]]
      val chips = items.filter(_.isInstanceOf[Chip]).asInstanceOf[Set[Chip]]
      gens.isEmpty || chips.forall(c => gens.exists(_.element == c.element))
    }
  }

  case class Config(elevator: Int, floors: Seq[Floor]) {
    lazy val curFloor = floors(elevator)

    def next(): Iterable[Config] = {
      def tries(newElevator: Int) = {
        val nextFloor = floors(newElevator)
        val newFloors = curFloor.movablePieces.map(p => curFloor.moveItems(nextFloor, p)).filter(f => f._1.valid && f._2.valid)
        newFloors.map(f => Config(newElevator, floors.updated(elevator, f._1).updated(newElevator, f._2)))
      }

      val low = if (elevator > 0) tries(elevator - 1) else Nil
      val high = if (elevator < floors.length - 1) tries(elevator + 1) else Nil
      (low ++ high).filterNot(_.seen)
    }

    private lazy val sig: String = {
      val elements = floors.zipWithIndex.flatMap(f => f._1.items.map((_, f._2))).groupBy(_._1.element)
      val positions = elements.values.map(i => (i.find(_._1.isInstanceOf[Generator]).get._2, i.find(_._1.isInstanceOf[Chip]).get._2))
      positions.map(p => s"$elevator.${p._1}.${p._2}").toSeq.sorted.mkString("-")
    }

    private val cache = mutable.Set[String]()

    private def seen: Boolean = {
      if (cache.contains(sig))
        true
      else {
        cache += sig
        false
      }
    }

    lazy val end: Boolean = floors.reverse.tail.forall(_.items.isEmpty)
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day11.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first,
      """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
        |The second floor contains a hydrogen generator.
        |The third floor contains a lithium generator.
        |The fourth floor contains nothing relevant.""", 11)

    println(first(lines))

    println(second(lines))
  }
}
