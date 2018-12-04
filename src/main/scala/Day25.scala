import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day25 {
  private var out = ArrayBuffer[Int]()

  def first(input: String): Int = {
    val instructions = input.split("\n").map(Instruction.parse)

    val chk = List.fill(50)(Seq(0, 1)).flatten

    def f(a: Int): Boolean = {
      val state = Machine(instructions, Map(Reg("a") -> a))
      out.clear()

      @tailrec def run(machine: Machine): Unit = {
        val next = machine.advance()
        if (out.length == chk.length)
          Unit
        else {
          next match {
            case None => Unit
            case Some(i) => run(i)
          }
        }
      }

      run(state)
      out.toList == chk
    }

    (0 until Integer.MAX_VALUE).find(f).get
  }

  def optimize(instructions: Seq[Instruction]): (Int, Instruction) = {
    val opt5: PartialFunction[Seq[Instruction], Instruction] = {
      case Seq(Inc(to), Dec(from1), Jnz(cmp1, Value(-2)), Dec(from2), Jnz(cmp2, Value(-5))) if from1 == cmp1 && from2 == cmp2 => Mul(from1, from2, to)
      case Seq(Inc(to), Inc(from1), Jnz(cmp1, Value(-2)), Inc(from2), Jnz(cmp2, Value(-5))) if from1 == cmp1 && from2 == cmp2 => Mul(from1, from2, to)
    }

    val opt3: PartialFunction[Seq[Instruction], Instruction] = {
      case Seq(Inc(to), Dec(from), Jnz(cmp, Value(-2))) if from == cmp => Add(from, to)
      case Seq(Dec(from), Inc(to), Jnz(cmp, Value(-2))) if from == cmp => Add(from, to)
    }

    val try5 = instructions.slice(0, 5)
    val try3 = instructions.slice(0, 3)
    if (opt5.isDefinedAt(try5)) (5, opt5(try5)) else if (opt3.isDefinedAt(try3)) (3, opt3(try3)) else (1, instructions.head)
  }

  case class Machine(instructions: Seq[Instruction], regs: Map[Reg, Int] = Map(), ip: Int = 0) {
    private val _regs = regs.withDefaultValue(0)

    def apply(reg: Reg): Int = _regs(reg)

    def updated(reg: Reg, value: Int): Machine = Machine(instructions, _regs.updated(reg, value), ip)

    def move(offset: Int): Machine = Machine(instructions, _regs, ip + offset)

    def advance(): Option[Machine] = {
      if (ip < 0 || ip >= instructions.length) None else {
        val subst = optimize(instructions.slice(ip, instructions.length))
        Some(subst._2.apply(this).move(subst._1))
      }
    }

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
    private val out = "out (.+)".r

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
        case out(x) => Out(parseRVal(x))
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

  case class Mul(from1: Reg, from2: Reg, to: Reg) extends Instruction {
    override def apply(state: Machine): Machine = state.updated(to, to.value(state) + from1.value(state) * from2.value(state)).updated(from1, 0).updated(from2, 0)
  }

  case class Out(from: RVal) extends Instruction {
    override def apply(state: Machine): Machine = {
      out += from.value(state)
      state
    }
  }

  def main(args: Array[String]): Unit = {
    val source = io.Source.fromFile("src/main/resources/day25.txt")
    val lines = try source.mkString finally source.close()

    println(first(lines))
  }
}
