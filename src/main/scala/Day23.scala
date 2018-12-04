import scala.annotation.tailrec

object Day23 {
  def first(input: String): Int = {
    val instructions = optimize(input.split("\n").map(Instruction.parse))
    val state = Machine(instructions, Map(Reg("a") -> 7))

    @tailrec def run(machine: Machine): Machine = {
      machine.current match {
        case None => machine
        case Some(i) => run(i.apply(machine).move(1))
      }
    }

    run(state)(Reg("a"))
  }

  def second(input: String): Int = {
    val instructions = optimize(input.split("\n").map(Instruction.parse))
    val state = Machine(instructions, Map(Reg("a") -> 12))

    @tailrec def run(machine: Machine): Machine = {
      println(f"ip: ${machine.ip} regs: ${machine.regs}")
      machine.current match {
        case None => machine
        case Some(i) => run(i.apply(machine).move(1))
      }
    }

    run(state)(Reg("a"))
  }

  def optimize(instructions: Seq[Instruction]): Seq[Instruction] = {
    case class AddPlaceHolder(index: Int, from: Reg, to: Reg)

    val newInstruction = (0 until instructions.length - 2).map(i => (instructions(i), instructions(i + 1), instructions(i + 2)) match {
      case (Inc(to), Dec(from), Jnz(cmp, Value(-2))) if from == cmp => Some(AddPlaceHolder(i, from, to))
      case (Dec(from), Inc(to), Jnz(cmp, Value(-2))) if from == cmp => Some(AddPlaceHolder(i, from, to))
      case _ => None
    }).find(_.isDefined).flatten

    if (newInstruction.isEmpty)
      return instructions

    val add = newInstruction.get

    val head = instructions.slice(0, add.index)
    val tail = instructions.slice(add.index + 3, instructions.length)
    head ++ Seq(Add(add.from, add.to), Nop(), Nop()) ++ optimize(tail)
  }

  case class Machine(instructions: Seq[Instruction], regs: Map[Reg, Int] = Map(), ip: Int = 0) {
    private val _regs = regs.withDefaultValue(0)

    def apply(reg: Reg): Int = _regs(reg)

    def updated(reg: Reg, value: Int): Machine = Machine(instructions, _regs.updated(reg, value), ip)

    def move(offset: Int): Machine = Machine(instructions, _regs, ip + offset)

    def current: Option[Instruction] = if (ip >= 0 && ip < instructions.length) Some(instructions(ip)) else None

    override def toString: String = s"[$ip] $regs"
  }

  abstract class RVal {
    def value(machine: Machine): Int
  }

  case class Reg(name: String) extends RVal {
    override def value(machine: Machine): Int = machine(this)
  }

  case class Value(value: Int) extends RVal {
    override def value(machine: Machine): Int = value
  }

  abstract sealed class Instruction {
    def apply(state: Machine): Machine
  }

  object Instruction {
    private val cpy = "cpy (.+) (.+)".r
    private val inc = "inc (.+)".r
    private val dec = "dec (.+)".r
    private val jnz = "jnz (.+) (.+)".r
    private val tgl = "tgl (.+)".r

    def parse(in: String): Instruction = {
      def parseRVal(in: String): RVal = {
        if (in.forall(c => c.isDigit || c == '-'))
          Value(in.toInt)
        else
          Reg(in)
      }

      in match {
        case cpy(from, to) => Cpy(parseRVal(from), Reg(to))
        case inc(reg) => Inc(Reg(reg))
        case dec(reg) => Dec(Reg(reg))
        case jnz(x, offset) => Jnz(parseRVal(x), parseRVal(offset))
        case tgl(x) => Tgl(parseRVal(x))
      }
    }
  }

  case class Cpy(from: RVal, to: Reg) extends Instruction {
    override def apply(state: Machine): Machine = state.updated(to, from.value(state))
  }

  case class Inc(reg: Reg) extends Instruction {
    override def apply(state: Machine): Machine = state.updated(reg, reg.value(state) + 1)
  }

  case class Dec(reg: Reg) extends Instruction {
    override def apply(state: Machine): Machine = state.updated(reg, reg.value(state) - 1)
  }

  case class Jnz(x: RVal, offset: RVal) extends Instruction {
    override def apply(state: Machine): Machine =
      if (x.value(state) != 0) state.move(offset.value(state) - 1) else state
  }

  case class Tgl(x: RVal) extends Instruction {
    override def apply(state: Machine): Machine = {
      val tgt = state.ip + x.value(state)
      if (tgt < 0 || tgt >= state.instructions.size) {
        state
      } else {
        val split = state.instructions.splitAt(tgt)

        val subst = split._2.head match {
          case Inc(reg) => Dec(reg)
          case Dec(reg) => Inc(reg)
          case Tgl(y @ Reg(_)) => Inc(y)
          case Jnz(v, to @ Reg(_)) => Cpy(v, to)
          case Cpy(from, to) => Jnz(from, to)
          case _ => Nop()
        }
        val newIns = (split._1 :+ subst) ++ split._2.tail
        Machine(newIns, state.regs, state.ip)
      }
    }
  }

  case class Add(from: Reg, to: Reg) extends Instruction {
    override def apply(state: Machine): Machine =
      state.updated(to, to.value(state) + from.value(state)).updated(from, 0)
  }

  case class Nop() extends Instruction {
    override def apply(state: Machine): Machine = state
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day23.txt")
    val lines = try source.mkString finally source.close()

    def test[T] = (fun: String => T, input: String, expected: T) => assert(fun(input) == expected, input)

    test(first, """cpy 2 a
                  |tgl a
                  |tgl a
                  |tgl a
                  |cpy 1 a
                  |dec a
                  |dec a""".stripMargin, 3)

    println(first(lines))

    println(second(lines))
  }
}
