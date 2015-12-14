import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle8Tests extends FlatSpec with Matchers {
  import Puzzle8._
  import Puzzle8.StringParsers._

  "Puzzle8" should "parse \"\"" in {
    val str = parseStr("\"\"")
    str.str should be ("\"\"")
    str.numCodeChars should be (2)
    str.numMemChars should be (0)
    str.numEncodedChars should be (6)
  }

  it should "parse \"abc\"" in {
    val str = parseStr("\"abc\"")
    str.str should be ("\"abc\"")
    str.numCodeChars should be (5)
    str.numMemChars should be (3)
    str.numEncodedChars should be (9)
  }

  it should "parse \"aaa\\\"aaa\"" in {
    val str = parseStr("\"aaa\\\"aaa\"")
    str.str should be ("\"aaa\"aaa\"")
    str.numCodeChars should be (10)
    str.numMemChars should be (7)
    str.numEncodedChars should be (16)
  }

  it should "parse \"\\x27" in {
    val str = parseStr("\"\\x27\"")
    str.str should be ("\"'\"")
    str.numCodeChars should be (6)
    str.numMemChars should be (1)
    str.numEncodedChars should be (11)
  }

  it should "solve part 1" in {
    val matchedStrs = input.map(str => parseStr(str))
    matchedStrs.size should be (300)
    val delta = matchedStrs.map(str => str.numCodeChars - str.numMemChars).sum
    delta should be (1342)
  }

  it should "solve part 2" in {
    val matchedStrs = input.map(str => parseStr(str))
    matchedStrs.size should be (300)
    val delta = matchedStrs.map(str => str.numEncodedChars - str.numCodeChars).sum
    delta should be (2074)
  }

  private val input: List[String] = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day8_input"))
    source.getLines().toList
  }
}
