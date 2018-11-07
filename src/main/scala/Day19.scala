import scala.annotation.tailrec

object Day19 {
  def first(input: Int): Int = {
    def turn(in: Seq[Int]): Seq[Int] = {
      val odd = in.grouped(2).map(_.head).toSeq
      if (in.length % 2 == 0)
        odd
      else
        odd.tail
    }

    @tailrec def last(from: Seq[Int]): Int = {
      if (from.length == 1)
        from.head
      else
        last(turn(from))
    }

    last(1 to input)
  }

  def second(input: Int): Int = {
    def turn(in: Seq[Int]): Seq[Int] = {
      val init = 3 - (in.length % 3)
      def keep(i: Int): Boolean = {
        i < in.length / 2 || (i + 1 - init) % 3 == 0
      }

      val filtered = in.indices.filter(keep).map(i => in(i))
      val (end, start) = filtered.splitAt(((in.length - 1) / 3) + 1)
      start ++ end
    }

    @tailrec def last(from: Seq[Int]): Int = {
      if (from.length == 2)
        from.head
      else
        last(turn(from))
    }

    last(1 to input)
  }

  def main(args: Array[String]): Unit = {
    def test[T] = (fun: Int => T, input: Int, expected: T) => assert(fun(input) == expected, input)

    test(first, 5, 3)

    println(first(3001330))

    test(second, 5, 2)

    println(second(3001330))
  }
}
