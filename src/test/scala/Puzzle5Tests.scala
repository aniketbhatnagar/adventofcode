import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle5Tests extends FlatSpec with Matchers {

  "Puzzle5" should "should pass example 1 in part 1" in {
    import Puzzle5.Part1Solver._
    validateNice("ugknbfddgicrmopn") should be (Right())
  }

  it should "should pass example 2 in part 1" in {
    import Puzzle5.Part1Solver._
    validateNice("aaa") should be (Right())
  }

  it should "should pass example 3 in part 1" in {
    import Puzzle5.Part1Solver._
    validateNice("jchzalrnumimnmhp") should be (Left(LetterNotRepeatedInARow))
  }

  it should "should pass example 4 in part 1" in {
    import Puzzle5.Part1Solver._
    validateNice("haegwjzuvuyypxyu") should be (Left(ContainsBlacklisted))
  }

  it should "should pass example 5 in part 1" in {
    import Puzzle5.Part1Solver._
    validateNice("dvszwmarrgswjxmb") should be (Left(ContainsLessThan3Vowels))
  }

  it should "solve part 1" in {
    import Puzzle5.Part1Solver._
    countNice(input) should be (236)
  }

  it should "should pass example 1 in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("qjhvhtzxzqqjkmpb") should be (Right())
  }

  it should "should pass example 2 in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("xxyxx") should be (Right())
  }

  it should "should pass example 3 in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("uurcxstgmygtbstg") should be (Left(LetterDoesntRepeatAfterOneLetter))
  }

  it should "should pass example 4 in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("ieodomkazucvgmuy") should be (Left(PairDoesNotRepeat))
  }

  it should "should not pass aaa in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("aaa") should be (Left(PairDoesNotRepeat))
  }

  it should "should pass aaaa in part 2" in {
    import Puzzle5.Part2Solver._
    validateNice("aaaa") should be (Right())
  }

  it should "solve part 2" in {
    import Puzzle5.Part2Solver._
    countNice(input) should be (51)
  }

  private val input: List[String] = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day5_input"))
    source.getLines().toList
  }
}
