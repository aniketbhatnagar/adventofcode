import org.scalatest.{Matchers, FlatSpec}

/**
 * @author aniket
 */
class Puzzle10Tests extends FlatSpec with Matchers {

  import Puzzle10._

  "Puzzle10" should "generate 11 sequence for 1" in {
    genSequence("1") should be ("11")
  }

  it should "generate 21 sequence for 11" in {
    genSequence("11") should be ("21")
  }

  it should "generate 21 sequence for 1211" in {
    genSequence("21") should be ("1211")
  }

  it should "generate 111221 sequence for 1211" in {
    genSequence("1211") should be ("111221")
  }

  it should "generate 312211 sequence for 111221" in {
    genSequence("111221") should be ("312211")
  }

  /*it should "solve the puzzle" in {
    val seq = genSequenceN("1113222113", 50)
    println(seq.length)
  }*/
}
