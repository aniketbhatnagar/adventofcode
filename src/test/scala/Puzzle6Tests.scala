import org.scalatest.{Matchers, FlatSpec}
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
 * @author aniket
 */
class Puzzle6Tests extends FlatSpec with Matchers {
  import Puzzle6._
  "InstructionParsers" should "parse turn on sample instruction" in {
    parseInstruction("turn on 887,9 through 959,629", InstructionParsers.instructionParser) should
      be (TurnOn(CoordinatesRange(Coordinates(887, 9), Coordinates(959, 629))))
  }

  it should "parse turn off sample instruction" in {
    parseInstruction("turn off 539,243 through 559,965", InstructionParsers.instructionParser) should
      be (TurnOff(CoordinatesRange(Coordinates(539, 243), Coordinates(559, 965))))
  }

  it should "parse toggle sample instruction" in {
    parseInstruction("toggle 720,196 through 897,994", InstructionParsers.instructionParser) should
      be (Toggle(CoordinatesRange(Coordinates(720, 196), Coordinates(897, 994))))
  }


  "VaryingBrightnessGrid" should "turn on all lights correctly" in {
    val lights = Array.fill(3 * 3)(0L)
    val expectedLights = Array.fill(3 * 3)(1L)
    VaryingBrightnessGrid(lights, 3, 3).turnOn(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    VaryingBrightnessGrid(lights, 3, 3).turnOn(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "turn off all lights correctly" in {
    val lights = Array.fill(3 * 3)(1L)
    val expectedLights = Array.fill(3 * 3)(0L)
    VaryingBrightnessGrid(lights, 3, 3).turnOff(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    VaryingBrightnessGrid(lights, 3, 3).turnOff(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "toggle all lights correctly" in {
    val lights = Array(0, 1, 2, 3, 4, 5, 6, 7, 8).map(_.toLong)
    val expectedLights = Array(2, 3, 4, 5, 6, 7, 8, 9, 10).map(_.toLong)
    VaryingBrightnessGrid(lights, 3, 3).toggle(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    VaryingBrightnessGrid(lights, 3, 3).toggle(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "turn on subset of lights correctly" in {
    val lights = Array.fill(4 * 4)(0L)
    val expectedLights = Array.tabulate(4 * 4)(i => {
      val row = i / 4
      val col = i % 4
      if (row >= 1 && col >= 1 && row <= 2 && col <= 2) {
        1L
      } else {
        0L
      }
    })
    VaryingBrightnessGrid(lights, 4, 4).turnOn(CoordinatesRange(Coordinates(1, 1), Coordinates(2, 2))).lights should be (expectedLights)
    VaryingBrightnessGrid(lights, 4, 4).turnOn(CoordinatesRange(Coordinates(2, 2), Coordinates(1, 1))).lights should be (expectedLights)
  }

  "OnOffGrid" should "turn on all lights correctly" in {
    val lights = Array.fill(3 * 3)(false)
    val expectedLights = Array.fill(3 * 3)(true)
    OnOffGrid(lights, 3, 3).turnOn(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    OnOffGrid(lights, 3, 3).turnOn(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "turn off all lights correctly" in {
    val lights = Array.fill(3 * 3)(true)
    val expectedLights = Array.fill(3 * 3)(false)
    OnOffGrid(lights, 3, 3).turnOff(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    OnOffGrid(lights, 3, 3).turnOff(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "toggle all lights correctly" in {
    val lights = Array(false, false, false, true, true, true, false, false, false)
    val expectedLights = Array(true, true, true, false, false, false, true, true, true)
    OnOffGrid(lights, 3, 3).toggle(CoordinatesRange(Coordinates(0, 0), Coordinates(2, 2))).lights should be (expectedLights)
    OnOffGrid(lights, 3, 3).toggle(CoordinatesRange(Coordinates(2, 2), Coordinates(0, 0))).lights should be (expectedLights)
  }

  it should "turn on subset of lights correctly" in {
    val lights = Array.fill(4 * 4)(false)
    val expectedLights = Array.tabulate(4 * 4)(i => {
      val row = i / 4
      val col = i % 4
      if (row >= 1 && col >= 1 && row <= 2 && col <= 2) {
        true
      } else {
        false
      }
    })
    OnOffGrid(lights, 4, 4).turnOn(CoordinatesRange(Coordinates(1, 1), Coordinates(2, 2))).lights should be (expectedLights)
    OnOffGrid(lights, 4, 4).turnOn(CoordinatesRange(Coordinates(2, 2), Coordinates(1, 1))).lights should be (expectedLights)
  }

  "Puzzle6" should "solve example 1 in part 1" in {
    countLightsOnAfter("turn on 0,0 through 999,999") should be (1000000)
  }

  it should "solve example 2 in part 1" in {
    countLightsOnAfter(
      """turn on 0,0 through 999,0
         |toggle 0,0 through 999,0
       """.stripMargin) should be (0)
  }

  it should "solve example 3 in part 1" in {
    countLightsOnAfter(
      """turn on 499,499 through 500,500
        |turn off 499,499 through 500,500
      """.stripMargin) should be (0)
  }

  it should "solve example 1 in part 2" in {
    sumBrightnessAfter(
      """turn on 0,0 through 0,0
        |turn on 0,0 through 0,0
      """.stripMargin) should be (2)
  }

  it should "solve example 2 in part 2" in {
    sumBrightnessAfter("toggle 0,0 through 999,999") should be (2000000)
  }

  it should "set total brightness to 3 after 1 turn on and 1 toggle in part 2" in {
    sumBrightnessAfter(
      """turn on 0,0 through 0,0
        |toggle 0,0 through 0,0
      """.stripMargin) should be (3)
  }

  it should "set total brightness to 0 after 1 turn on, 1 toggle and 3 turn offs in part 2" in {
    sumBrightnessAfter(
      """turn on 0,0 through 0,0
        |turn off 0,0 through 0,0
        |toggle 0,0 through 0,0
        |turn off 0,0 through 0,0
        |turn off 0,0 through 0,0
      """.stripMargin) should be (0)
  }

  it should "set total brightness to 0 after 1 turn on and 2 turn offs in part 2" in {
    sumBrightnessAfter(
      """turn on 0,0 through 0,0
        |turn off 0,0 through 0,0
        |turn off 0,0 through 0,0
      """.stripMargin) should be (0)
  }

  it  should "solve part 1" in {
    countLightsOnAfter(input) should be (377891)
  }

  it should "solve part 2" in {
    sumBrightnessAfter(input) should be (14110788L)
  }

  private def parseInstruction[Instruction](e: String, parser: InstructionParsers.Parser[Instruction]): Instruction = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = InstructionParsers.phrase(parser)
    //we need to wrap the expression string in a reader so our parser can digest it
    val input = new CharSequenceReader(e)
    phraseParser(input) match {
      case InstructionParsers.Success(expression, _) => expression
      case InstructionParsers.NoSuccess(msg, _) => throw new IllegalArgumentException(s"Could not parser expression $e. Error - $msg")
    }
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day6_input"))
    source.mkString
  }

}
