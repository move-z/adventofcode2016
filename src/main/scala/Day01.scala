import scala.annotation.tailrec
import scala.collection.mutable

object Day01 {
  def first(input: String): Int = {
    val steps = input.stripLineEnd.split(", *").map(parse)
    val endPos = steps.foldLeft(Position(Location(0, 0), Up))((cur, step) => cur.advance(step))
    Math.abs(endPos.location.x) + Math.abs(endPos.location.y)
  }

  def second(input: String): Int = {
    val steps = input.stripLineEnd.split(", *").map(parse)
    val visited: mutable.Set[Location] = mutable.Set()
    val start = Position(Location(0, 0), Up)
    visited += start.location
    @tailrec def walk(position: Position, steps: Seq[Step]): Location = {
      val next = steps.head
      if (next.amount > 1) {
        val one = Step(next.direction, 1)
        val two = Step(Null, next.amount - 1)
        walk(position, one +: two +: steps.tail)
      } else {
        val newPos = position.advance(next)
        if (visited.contains(newPos.location))
          newPos.location
        else {
          visited += newPos.location
          walk(newPos, steps.tail)
        }
      }
    }
    val firstVisited = walk(start, steps)
    Math.abs(firstVisited.x) + Math.abs(firstVisited.y)
  }

  private def parse(s: String) = {
    val dir = s.charAt(0) match {
      case 'R' => Right
      case 'L' => Left
    }
    val amount = s.substring(1).toInt
    Step(dir, amount)
  }

  private case class Step(direction: Direction, amount: Int)

  private case class Location(x: Int, y: Int) {
    def advance(amount: Int, heading: Heading): Location = {
      heading match {
        case Up => Location(x, y + amount)
        case Down => Location(x, y - amount)
        case East => Location(x + amount, y)
        case West => Location(x - amount, y)
      }
    }
  }

  private case class Position(location: Location, heading: Heading) {
    def advance(step: Step): Position = {
      val newHead = heading.turn(step.direction)
      val newLocation = location.advance(step.amount, newHead)
      Position(newLocation, newHead)
    }
  }

  private sealed trait Direction
  private object Left extends Direction
  private object Right extends Direction
  private object Null extends Direction

  private sealed trait Heading {
    def turn(direction: Direction): Heading = {
      (this, direction) match {
        case (Up, Left) => West
        case (Up, Right) => East
        case (Down, Left) => East
        case (Down, Right) => West
        case (East, Left) => Up
        case (East, Right) => Down
        case (West, Left) => Down
        case (West, Right) => Up
        case (d, Null) => d
      }
    }
  }
  private object Up extends Heading
  private object Down extends Heading
  private object East extends Heading
  private object West extends Heading

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day01.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, "R2, L3", 5)
    test(first, "R2, R2, R2", 2)
    test(first, "R5, L5, R5, R3", 12)
    println(first(lines))

    test(second, "R8, R4, R4, R8", 4)
    println(second(lines))
  }
}
