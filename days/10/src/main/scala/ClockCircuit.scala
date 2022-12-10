import ClockCircuit.{Effect, LoomingEffect}
import better.files.Resource

import scala.util.matching.Regex

object ClockCircuit {
  enum Register:
    case X

  sealed trait Effect(val durationInCycles: Int)
  case object NoOperation extends Effect(1)
  case class AddRegister(register: Register, value: BigInt) extends Effect(2)

  case class LoomingEffect(effect: Effect, due: Int)

  def init(instructions: Seq[Effect]): ClockCircuit = ClockCircuit(0, Map(Register.X -> _1), instructions.toList, List.empty)

  private val noOpRegex: Regex = """noop""".r
  private val addXRegex: Regex = """addx (-?\d+)""".r
  def parseInput(input: String): Seq[Effect] = input.split("\n").map(parseLine)
  private def parseLine(line: String): Effect = line match
    case noOpRegex() => NoOperation
    case addXRegex(param) => AddRegister(Register.X, BigInt(param.toInt))
    case _ => throw IllegalArgumentException(s"Failure reading unknown instruction: '$line'")

  extension (states: LazyList[ClockCircuit])
    def signalStrength: LazyList[BigInt] = states.zipWithIndex
      .map(t => (t._1, t._2 + 1))
      .filter(t => t._2 % 40 == 20)
      .map(t => t._2 * t._1.x)

  private val _0: BigInt = BigInt(0)
  private val _1: BigInt = BigInt(1)
}

case class ClockCircuit(
  clock: Int,
  registers: Map[ClockCircuit.Register, BigInt],
  instructions: List[Effect],
  looming: List[LoomingEffect],
 ) {
  import ClockCircuit._

  def x: BigInt = registers(Register.X)
  /** Stabilized means that there is no working in progress or left to do and no effect is looming */
  def isStabilized: Boolean = instructions.isEmpty && looming.isEmpty

  private def applyEffect(effect: Effect): ClockCircuit = effect match
    case NoOperation => this
    case AddRegister(r, value) =>
      copy(registers = this.registers.updatedWith(r)(rv => Option(rv.getOrElse(_0) + value)))

  def tick: ClockCircuit =
    val timePassed = copy(
      clock = clock + 1,  // time passed
      looming = looming.map(l => l.copy(due = l.due - 1)) // and every looming effect comes closer to being due
    )
    val (dues, stillLooming) = timePassed.looming.partition(_.due == 0)  // does this preserve order?
    val changed = dues.map(_.effect).foldLeft(timePassed)(_ applyEffect _).copy(looming = stillLooming)
    if (stillLooming.nonEmpty) changed else changed.takeNextInstruction // Only begin working on the next command, if there is no one currently processed

  def ticks: LazyList[ClockCircuit] = LazyList.unfold(this) { c =>
    Option(c.tick).filter(n => !n.isStabilized || c.registers != n.registers).map(x => (x, x)) }

  private def takeNextInstruction: ClockCircuit = copy(
    instructions = this.instructions.drop(1),
    looming = instructions.headOption match
      case None => looming
      case Some(instr) => looming :+ LoomingEffect(instr, instr.durationInCycles)
  )

  override def toString: String = f"Circuit(cycle:$clock%03d | x=$x%3d | instructions: ${instructions.size}%2d | looming: ${looming.size}%2d)"
}

def input: String = Resource.asString("inputA.txt").getOrElse(sys.error("Couldn't read input file"))

@main def part1 =
  val circuit = ClockCircuit.init(ClockCircuit.parseInput(input))
  val signals = circuit.ticks.signalStrength
  println("Received signals:")
  val result = signals.tapEach(println).sum
  println(s"For a total signal of $result")

@main def part2 =
  val circuit = ClockCircuit.init(ClockCircuit.parseInput(input))
  val image = circuit.ticks.foldLeft("") { (res, circuit) =>
    res + (if ((circuit.clock - 1) % 40 - circuit.x).abs <= 1 then "#" else ".")
  }.grouped(40).mkString("\n")
  println(image)