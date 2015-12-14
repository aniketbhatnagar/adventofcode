import org.scalatest.{Matchers, FlatSpec}

/**
 * @author aniket
 */
class Puzzle11Tests extends FlatSpec with Matchers {

  import Puzzle11._

  "Puzzle11" should "detect one increasing straight of at least three letters" in {
    validate3StraightChars("hijklmmn") should be (Right())
  }

  it should "fail when no increasing straight of at least three letters" in {
    validate3StraightChars("abbceffg") should be (Left(NoThreeIncreasingLetters))
  }

  it should "detect 2 different pairs" in {
    validate2DifferentPairs("abbceffg") should be (Right())
  }

  it should "fail when no 2 different pairs" in {
    validate2DifferentPairs("abbcegjk") should be (Left(NoTwoPairs))
  }

  it should "fail when i or o or l exists" in {
    validateNoIOLChars("hijklmmn") should be (Left(NoIOLAllowed))
  }

  it should "fail hijklmmn because it contains i and l" in {
    validate("hijklmmn") should be (Left(NoIOLAllowed))
  }

  it should "fail abbceffg because no increasing straight of at least three letters" in {
    validate("abbceffg") should be (Left(NoThreeIncreasingLetters))
  }

  it should "fail abbcegjk because no 2 different pairs" in {
    validate("abbcdegjk") should be (Left(NoTwoPairs))
  }

  "passwordsFrom" should "generate abcdefgg after abcdefgh" in {
    passwordsFrom("abcdefgg").take(1).toList(0) should be ("abcdefgh")
  }

  it should "generate abcdefz after abcdega" in {
    passwordsFrom("abcdefz").take(1).toList(0) should be ("abcdega")
  }

  it should "generate zzzz after aaaaa" in {
    passwordsFrom("zzzz").take(1).toList(0) should be ("aaaaa")
  }

  "Puzzle11" should "generate next password after abcdefgh as abcdffaa" in {
    nextValidPassword("abcdefgh") should be ("abcdffaa")
  }

  it should "solve part 1" in {
    nextValidPassword("vzbxkghb") should be ("vzbxxyzz")
  }

  it should "solve part 2" in {
    nextValidPassword("vzbxxyzz") should be ("vzcaabcc")
  }
}
