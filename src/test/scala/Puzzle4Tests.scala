import org.scalatest.{Matchers, FlatSpec}

/**
 * @author aniket
 */
class Puzzle4Tests extends FlatSpec with Matchers {

  import Puzzle4._

  "MD5" should "hash abcdef609043 to 000001dbbfa3a5c83a2d506429c7b00e" in {
    hash("abcdef609043") should be ("000001dbbfa3a5c83a2d506429c7b00e")
  }

  "Puzzle 4" should "mine correctly for example 1 in part 1" in {
    mine("abcdef", 609042) should be (609043)
  }

  it should "mine correctly for example 2 in part 1" in {
    mine("pqrstuv", 1048969) should be (1048970)
  }

  it should "solve part 1" in {
    mine(secret, 117900) should be (117946)
  }

  it should "solve part 2" in {
    mine(secret, 3938000, 6) should be (3938038)
  }

  private val secret = "ckczppom"
}
