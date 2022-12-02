import better.files.Resource
import Play.*
import GameResult.*


enum GameResult:
    case Win
    case Loss
    case Draw

    def score: Int = this match
        case Win => 6
        case Loss => 0
        case Draw => 3

object GameResult:
    def fromString(input: String): GameResult = input match 
        case "X" => GameResult.Loss
        case "Y" => GameResult.Draw
        case "Z" => GameResult.Win
        case unknown => throw new IllegalArgumentException(s"Illegal argument for a game result: '$unknown'!")


enum Play:
    case Rock
    case Paper
    case Scissor

    def score: Int = this match
        case Rock => 1
        case Paper => 2
        case Scissor => 3
    
object Play:
    def fromString(input: String): Play = input match 
        case "A" | "X" => Play.Rock
        case "B" | "Y" => Play.Paper
        case "C" | "Z" => Play.Scissor
        case unknown => throw new IllegalArgumentException(s"Illegal argument for a play: '$unknown'!")

extension (tuple: (Play, Play))
    def isDraw: Boolean = (tuple._1 == tuple._2)
    def result: GameResult = 
        val (opponentPlay, ownPlay) = tuple
        if (isDraw) GameResult.Draw
        else 
            val isWon = opponentPlay match
                case Play.Rock => ownPlay == Play.Paper
                case Play.Paper => ownPlay == Play.Scissor
                case Play.Scissor => ownPlay == Play.Rock
            if isWon then GameResult.Win else GameResult.Loss 
    def isWon: Boolean = result == GameResult.Win
    def isLoss: Boolean = result == GameResult.Draw

extension (tuple: (Play, GameResult))
    def requiredPlay: Play = tuple match
        case (Rock, Win) => Paper
        case (Rock, Draw) => Rock
        case (Rock, Loss) => Scissor
        case (Paper, Win) => Scissor
        case (Paper, Draw) => Paper
        case (Paper, Loss) => Rock
        case (Scissor, Win) => Rock
        case (Scissor, Draw) => Scissor
        case (Scissor, Loss) => Paper

def readPlays[X,Y](input: String)(part1: String => X, part2: String => Y): List[(X, Y)] = {
  input.split("\n").map { line =>
    val parts = line.split(" ")
    if (parts.size != 2) throw new java.lang.IllegalArgumentException(s"Illegal line with ${parts.size} elements instead of 2 plays!")
    (part1(parts(0)), part2(parts(1)))
  }.toList
}

def score(plays: List[(Play, Play)]) = plays.map(_.result.score).sum + plays.map(_._2.score).sum
        
@main def part1(): Unit = {
  val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
  val theScore = score(readPlays(input)(Play.fromString, Play.fromString))
  println(s"Score according to strategy play is: $theScore")
}

@main def part2(): Unit = {
  val input = Resource.asString("inputA.txt").getOrElse(throw new IllegalStateException("Couldn't read input file"))
  val playAndResults = readPlays(input)(Play.fromString, GameResult.fromString)
  val plays = playAndResults.map(t => (t._1, t.requiredPlay))
  val theScore = score(plays)
  println(s"Score according to strategy play is: $theScore")
}