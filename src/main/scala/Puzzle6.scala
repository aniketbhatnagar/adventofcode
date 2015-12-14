import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 6: Probably a Fire Hazard ---
 *
 * http://adventofcode.com/day/6
 *
 * @author aniket
 */
object Puzzle6 {

  def countLightsOnAfter(instructionsString: String): Int = {
    val initialGrid = OnOffGrid(Array.fill(GRID_HEIGHT_IN_PUZZLE * GRID_WIDTH_IN_PUZZLE)(false), GRID_HEIGHT_IN_PUZZLE, GRID_WIDTH_IN_PUZZLE)
    val grid = parseAndProcessInstructions(instructionsString, initialGrid)
    grid.lights.count(light => light)
  }

  def sumBrightnessAfter(instructionsString: String): Long = {
    val initialGrid = VaryingBrightnessGrid(Array.fill(GRID_HEIGHT_IN_PUZZLE * GRID_WIDTH_IN_PUZZLE)(0L), GRID_HEIGHT_IN_PUZZLE, GRID_WIDTH_IN_PUZZLE)
    val grid = parseAndProcessInstructions(instructionsString, initialGrid)
    grid.lights.sum
  }

  def parseAndProcessInstructions[T](instructionsString: String, initialGrid: Grid[T]): Grid[T] = {
    val instructions = parseInstructions(instructionsString)
    processInstructions(instructions, initialGrid)
  }

  def processInstructions[T](instructions: List[Instruction], initialGrid: Grid[T]): Grid[T] = {
    instructions.foldLeft(initialGrid)((grid, instruction) => {
      instruction match {
        case TurnOn(range)  => grid.turnOn(range)
        case TurnOff(range) => grid.turnOff(range)
        case Toggle(range)  => grid.toggle(range)
      }
    })
  }

  private def parseInstructions(instructionsString: String): List[Instruction] = {
    import InstructionParsers._
    val input = new CharSequenceReader(instructionsString)
    instructionsParser(input) match {
      case Success(instructions, _) => instructions
      case NoSuccess(msg, _)        => throw new IllegalArgumentException(s"Parsing failed: $msg")
    }
  }

  private val GRID_HEIGHT_IN_PUZZLE = 1000
  private val GRID_WIDTH_IN_PUZZLE = 1000

  trait Grid[T] {
    require(lights.size == (numCols * numRows))

    def lights: Array[T] // mutable for performance reasons.
    def numRows: Int
    def numCols: Int

    def turnOn(range: CoordinatesRange): Grid[T]
    def turnOff(range: CoordinatesRange): Grid[T]
    def toggle(range: CoordinatesRange): Grid[T]

    protected def updated(range: CoordinatesRange, updateF: T => T): Grid[T] = {
      for {
        x <- (Math.min(range.from.x, range.to.x)) to (Math.max(range.from.x, range.to.x))
        y <- (Math.min(range.from.y, range.to.y)) to (Math.max(range.from.y, range.to.y))
      } {
        lights(x + numCols * y) = updateF(lights(x + numCols * y))
      }
      this
    }
  }

  case class VaryingBrightnessGrid(private val _lights: Array[Long], val numRows: Int, val numCols: Int) extends Grid[Long] {

    override lazy val lights: Array[Long] = _lights.clone()

    override def turnOn(range: CoordinatesRange): Grid[Long] = {
      updated(range, light => light + 1)
    }

    override def turnOff(range: CoordinatesRange): Grid[Long] = {
      updated(range, light => {
        if (light == 0) {
          light
        } else {
          light - 1
        }
      })
    }

    override def toggle(range: CoordinatesRange): Grid[Long] = {
      updated(range, light => light + 2)
    }
  }

  case class OnOffGrid(_lights: Array[Boolean], numRows: Int, numCols: Int) extends Grid[Boolean] {

    override lazy val lights: Array[Boolean] = _lights.clone()

    override def turnOn(range: CoordinatesRange): Grid[Boolean] = {
      updated(range, _ => true)
    }

    override def turnOff(range: CoordinatesRange): Grid[Boolean] = {
      updated(range, _ => false)
    }

    override def toggle(range: CoordinatesRange): Grid[Boolean] = {
      updated(range, light => !light)
    }
  }

  case class Coordinates(x: Int, y: Int)
  case class CoordinatesRange(from: Coordinates, to: Coordinates)

  sealed trait Instruction
  case class TurnOn(range: CoordinatesRange) extends Instruction
  case class TurnOff(range: CoordinatesRange) extends Instruction
  case class Toggle(range: CoordinatesRange) extends Instruction

  object InstructionParsers extends JavaTokenParsers {
    val turn: Parser[String] = "turn"
    val on: Parser[String] = "on"
    val off: Parser[String] = "off"
    val toggle: Parser[String] = "toggle"
    val through: Parser[String] = "through"

    val coordinates: Parser[Coordinates] = (
        (wholeNumber <~ ",".r) ~ wholeNumber
      ).map(numbers => Coordinates(numbers._1.toInt, numbers._2.toInt))

    val coordinatesRange: Parser[CoordinatesRange] = (
        (coordinates <~ through) ~ coordinates
      ).map(coordinatesPair => CoordinatesRange(coordinatesPair._1, coordinatesPair._2))

    val turnOnInstruction: Parser[TurnOn] = ((turn ~ on) ~> coordinatesRange).map(range => TurnOn(range))
    val turnOffInstruction: Parser[TurnOff] = ((turn ~ off) ~> coordinatesRange).map(range => TurnOff(range))
    val toggleInstruction: Parser[Toggle] = (toggle ~> coordinatesRange).map(range => Toggle(range))
    val instructionParser: Parser[Instruction] = turnOnInstruction | turnOffInstruction | toggleInstruction
    val instructionsParser: Parser[List[Instruction]] = instructionParser*
  }
}
