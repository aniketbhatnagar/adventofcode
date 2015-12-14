import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle2Tests extends FlatSpec with Matchers {

  import Puzzle2._

  "Rectangle" should "calculate surface area correctly" in {
    Rectangle(2, 3).surfaceArea should be (6)
    Rectangle(1, 3).surfaceArea should be (3)
    Rectangle(0, 3).surfaceArea should be (0)
  }

  "Rectangle" should "calculate perimeter correctly" in {
    Rectangle(2, 3).perimeter should be (10)
    Rectangle(1, 1).perimeter should be (4)
  }

  "Box" should "calculate surface area correctly" in {
    Box(2, 3, 4).surfaceArea should be(52)
    Box(1, 1, 10).surfaceArea should be(42)
  }

  "Box" should "calculate volume correctly" in {
    Box(2, 3, 4).volume should be(24)
    Box(1, 1, 10).volume should be(10)
  }

  "Puzzle2" should "calculate required wrapping area for example 1 in part 1 correctly" in {
    calcRequiredPaper(Box(2, 3, 4)) should be (58)
  }

  "Puzzle2" should "calculate required wrapping area for example 2 in part 1 correctly" in {
    calcRequiredPaper(Box(1, 1, 10)) should be (43)
  }

  "Puzzle2" should "solve the puzzle with provided input part 1" in {
    calcRequiredPaper(inputBoxes) should be (1598415)
  }

  "Puzzle2" should "calculate required wrapping area for example 1 in part 2 correctly" in {
    calcRequiredRibbon(Box(2, 3, 4)) should be (34)
  }

  "Puzzle2" should "solve the puzzle with provided input part 2" in {
    calcRequiredRibbon(inputBoxes) should be (3812909)
  }

  private val input: List[String] = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day2_input"))
    source.getLines().toList
  }

  private val inputBoxes: List[Box] = {
    input.map(line => {
      val lineSplits = line.split("x")
      Box(lineSplits(0).toInt, lineSplits(1).toInt, lineSplits(2).toInt)
    })
  }
}
