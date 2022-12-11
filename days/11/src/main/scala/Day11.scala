import Expr.Multiplication
import better.files.Resource

import java.util
import scala.annotation.targetName

enum Expr:
  case Literal(no: Int)
  case Reference
  case Addition(left: Expr, right: Expr)
  case Multiplication(left: Expr, right: Expr)
  def evaluate(old: BigInt): BigInt = this match
    case Literal(no) => no
    case Reference => old
    case Addition(left, right) => left.evaluate(old) + right.evaluate(old)
    case Multiplication(left, right) => left.evaluate(old) * right.evaluate(old)
  override def toString: String = this match
    case Literal(no) => Integer.toString(no)
    case Reference => "old"
    case Addition(l, r) => s"${l.toString} + ${r.toString}"
    case Multiplication(l, r) => s"${l.toString} * ${r.toString}"

object Expr:
  def parse(line: String): Expr =
    val tokens = line.split(" ")
    val (left, right) = (parseToken(tokens(0)), parseToken(tokens(2)))
    tokens(1).trim match
      case "+" => Expr.Addition(left, right)
      case "*" => Expr.Multiplication(left, right)
  private def parseToken(token: String): Expr = token.trim match
    case number if number.forall(_.isDigit) => Expr.Literal(number.toInt)
    case "old" => Expr.Reference
    case _ => sys.error(s"Error parsing operator expression. Illegal token '$token'!")

case class Monkey(
  no: Int,
  items: List[BigInt],
  expr: Expr,
  divisor: Int,
  target: (Int, Int),
) {
  def intensifyWorry(old: BigInt): BigInt = expr.evaluate(old)
  def targetForWorryLevel(lvl: BigInt): Int = if lvl % divisor == 0 then target._2 else target._1
  def dropItem: Monkey = copy(items = this.items.drop(1))
  def receiveItem(worryAbout: BigInt): Monkey = copy(items = this.items :+ worryAbout)

  override def toString: String = f"Monkey($no%02d holds ${items.mkString(", ")} | new = $expr | check: x / $divisor?)"
}

object Monkey:
  def parse(input: String, expectedNo: Int): Monkey =
    val lines = input.split("\n")
    lines.head match
      case lineMonkey(no) => require(no.toInt == expectedNo, s"Expected to read monkey $expectedNo but got $no instead!")
    val items = lines(1) match
      case lineItems(itemline) => itemline.split(",").map(_.trim.toInt).map(BigInt.apply).toList
    val opExpression = lines(2) match
      case lineOp(operationLine) => Expr.parse(operationLine)
    val divisor = lines(3) match
      case lineTest(number) => number.toInt
    val targetTrue = lines(4) match
      case lineTestTrue(no) => no.toInt
    val targetFalse = lines(5) match
      case lineTestFalse(no) => no.toInt
    require(targetTrue != expectedNo, s"Bored monkey $expectedNo cannot throw an item to itself!")
    require(targetFalse != expectedNo, s"Bored monkey $expectedNo cannot throw an item to itself!")
    Monkey(
      no = expectedNo,
      items = items,
      expr = opExpression,
      divisor = divisor,
      target = (targetFalse, targetTrue)
    )
  private val lineMonkey = """Monkey (\d+):""".r
  private val lineItems = """\s*Starting items: (.*)""".r
  private val lineOp = """\s*Operation: new = (.*)""".r
  private val lineTest = """\s*Test: divisible by (\d+)""".r
  private val lineTestTrue = """\s*If true: throw to monkey (\d+)""".r
  private val lineTestFalse = """\s*If false: throw to monkey (\d+)""".r

def parseMonkeys(input: String): Vector[Monkey] =
  input.split("\n\n").zipWithIndex.map(Monkey.parse.tupled).toVector

case class MonkeyPlayfield(monkeys: Vector[Monkey], inspectionCount: Map[Int, Int] = Map.empty.withDefaultValue(0), relieveFactor: BigInt, normalizer: BigInt => BigInt = identity) {

  def round: MonkeyPlayfield = monkeys.indices.foldLeft(this) { (field, monkeyNo) =>
    val monkey = field.monkeys(monkeyNo)
    monkey.items.foldLeft(field) { (itemField, item) => // His items do not change, because monkeys don't throw to themselves
      val newWorryLevel = normalizer(monkey.intensifyWorry(item) / relieveFactor)
      val targetMonkey = monkey.targetForWorryLevel(newWorryLevel)
      itemField.copy(
        inspectionCount = itemField.increasedInspectionFor(monkey.no),
        monkeys = itemField.monkeys
          .updated(monkey.no, itemField.monkeys(monkey.no).dropItem)
          .updated(targetMonkey, itemField.monkeys(targetMonkey).receiveItem(newWorryLevel))
      )
    }
  }

  def rounds: LazyList[MonkeyPlayfield] = LazyList.iterate(this)(_.round)
  def monkeyBusinesses: List[Int] = inspectionCount.values.toList
  def monkeyBusinessLevel: BigInt = monkeyBusinesses.sorted.reverse.take(2).map(BigInt.apply).product

  def inspectItems(round: Int): String =
    val header = s"""After round $round, the monkeys are holding items with these worry levels"""
    val monkeyLines = monkeys.map(m => f"Monkey ${m.no}%d: ${m.items.mkString(", ")}")
    (header +: monkeyLines :+ "").mkString("\n")
  def inspectMonkeyBusiness(round: Int): String =
    val header = s"== After round $round =="
    val monkeyLines = inspectionCount.toList.sortBy(_._1).map((no, count) => s"Monkey $no inspected items $count times")
    (header +: monkeyLines :+ "").mkString("\n")

  private def increasedInspectionFor(no: Int): Map[Int, Int] =
    inspectionCount.updatedWith(no)(prev => prev.map(_ + 1).orElse(Some(1)))
}

object MonkeyPlayfield {
  def initializeWithNormalizer(monkeys: Vector[Monkey]): MonkeyPlayfield =
    val commonGroup = monkeys.map(_.divisor).product
    MonkeyPlayfield(monkeys, relieveFactor = 1, normalizer = (n: BigInt) => n % commonGroup)
}

def input: String = Resource.asString("inputA.txt").getOrElse(sys.error("Couldn't read input file"))

@main def part1 =
  val monkeyField = MonkeyPlayfield(parseMonkeys(input), relieveFactor = 3)
  println(s"The level of monkey business after 20 rounds is ${monkeyField.rounds(20).monkeyBusinessLevel}")

@main def part2 =
  val monkeyField = MonkeyPlayfield.initializeWithNormalizer(parseMonkeys(input))
  println(s"The level of monkey business after 10000 rounds is ${monkeyField.rounds(10000).monkeyBusinessLevel}")
