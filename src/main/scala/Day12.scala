object Day12 {
  def first(input: String): Int = {
    val instructions = input.split("\n")
    state = State()

    while (state.ip >= 0 && state.ip < instructions.length) {
      val instruction = Instruction.parse(instructions(state.ip))
      state = instruction.apply()
    }

    state(Reg("a"))
  }

  def second(input: String): Int = {
    val instructions = input.split("\n")
    state = State().updated(Reg("c"), 1)

    while (state.ip >= 0 && state.ip < instructions.length) {
      val instruction = Instruction.parse(instructions(state.ip))
      state = instruction.apply()
      println(state)
    }

    state(Reg("a"))
  }

  var state = State()

  case class State(regs: Map[Reg, Int] = Map(), ip: Int = 0) {
    private val _regs = regs.withDefaultValue(0)

    def apply(reg: Reg): Int = _regs(reg)

    def updated(reg: Reg, value: Int): State = State(_regs.updated(reg, value), ip)

    def move(offset: Int): State = State(_regs, ip + offset)
  }

  abstract class RVal {
    val value: Int
  }

  case class Reg(name: String) extends RVal {
    override lazy val value: Int = state(this)
  }

  case class Value(override val value: Int) extends RVal

  abstract sealed class Instruction {
    def apply(): State
  }

  object Instruction {
    private val cpy = "cpy (.+) (.+)".r
    private val inc = "inc (.+)".r
    private val dec = "dec (.+)".r
    private val jnz = "jnz (.+) (.+)".r

    def parse(in: String): Instruction = {
      def parseRVal(in: String): RVal = {
        if (in.forall(_.isDigit))
          Value(in.toInt)
        else
          Reg(in)
      }

      in match {
        case cpy(from, to) => Cpy(parseRVal(from), Reg(to))
        case inc(reg) => Inc(Reg(reg))
        case dec(reg) => Dec(Reg(reg))
        case jnz(x, offset) => Jnz(parseRVal(x), offset.toInt)
      }
    }
  }

  case class Cpy(from: RVal, to: Reg) extends Instruction {
    override def apply(): State = state.updated(to, from.value).move(1)
  }

  case class Inc(reg: Reg) extends Instruction {
    override def apply(): State = state.updated(reg, state(reg) + 1).move(1)
  }

  case class Dec(reg: Reg) extends Instruction {
    override def apply(): State = state.updated(reg, state(reg) - 1).move(1)
  }

  case class Jnz(x: RVal, offset: Int) extends Instruction {
    override def apply(): State = if (x.value != 0) state.move(offset) else state.move(1)
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day12.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, """cpy 41 a
                  |inc a
                  |inc a
                  |dec a
                  |jnz a 2
                  |dec a
                  |""".stripMargin, 42)

    println(first(lines))

    println(second(lines))
  }
}
