object Day12 {
  def first(input: String): Int = {
    val instructions = optimize(input.split("\n").map(Instruction.parse))
    state = State()

    while (state.ip >= 0 && state.ip < instructions.length) {
      val instruction = instructions(state.ip)
      state = instruction.apply().move(1)
    }

    state(Reg("a"))
  }

  def second(input: String): Int = {
    val instructions = optimize(input.split("\n").map(Instruction.parse))
    state = State().updated(Reg("c"), 1)

    while (state.ip >= 0 && state.ip < instructions.length) {
      val instruction = instructions(state.ip)
      state = instruction.apply().move(1)
    }

    state(Reg("a"))
  }

  def optimize(instructions: Seq[Instruction]): Seq[Instruction] = {
    case class AddPlaceHolder(index: Int, from: Reg, to: Reg)

    val newInstruction = (0 until instructions.length - 2).map(i => (instructions(i), instructions(i + 1), instructions(i + 2)) match {
      case (Inc(to), Dec(from), Jnz(cmp, -2)) if from == cmp => Some(AddPlaceHolder(i, from, to))
      case _ => None
    }).find(_.isDefined).flatten

    if (newInstruction.isEmpty)
      return instructions

    val add = newInstruction.get

    val head = instructions.slice(0, add.index)
    val newHead = head.indices.map(i => head(i) match {
        case Jnz(x, offset) if offset > add.index - i => Jnz(x, offset - 2)
        case s => s
      })
    val h = newHead :+ Add(add.from, add.to)

    val tail = instructions.slice(add.index + 3, instructions.length)
    val newTail = tail.indices.map(i => tail(i) match {
      case Jnz(x, offset) if -offset > i => Jnz(x, offset + 2)
      case s => s
    })

    h ++ optimize(newTail)
  }

  var state = State()

  case class State(regs: Map[Reg, Int] = Map(), ip: Int = 0) {
    private val _regs = regs.withDefaultValue(0)

    def apply(reg: Reg): Int = _regs(reg)

    def updated(reg: Reg, value: Int): State = State(_regs.updated(reg, value), ip)

    def move(offset: Int): State = State(_regs, ip + offset)

    override def toString: String = s"[$ip] $regs"
  }

  abstract class RVal {
    def value: Int
  }

  case class Reg(name: String) extends RVal {
    override def value: Int = state(this)
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
    override def apply(): State = state.updated(to, from.value)
  }

  case class Inc(reg: Reg) extends Instruction {
    override def apply(): State = state.updated(reg, reg.value + 1)
  }

  case class Dec(reg: Reg) extends Instruction {
    override def apply(): State = state.updated(reg, reg.value - 1)
  }

  case class Jnz(x: RVal, offset: Int) extends Instruction {
    override def apply(): State = if (x.value != 0) state.move(offset - 1) else state
  }

  case class Add(from: Reg, to: Reg) extends Instruction {
    override def apply(): State = state.updated(to, to.value + from.value).updated(from, 0)
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
