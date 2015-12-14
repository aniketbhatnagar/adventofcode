import java.security.MessageDigest

/**
 * --- Day 4: The Ideal Stocking Stuffer ---
 *
 * http://adventofcode.com/day/4
 *
 * @author aniket
 */
object Puzzle4 {

  def mine(secret: String, from: Int = DEFAULT_FROM, numZeros: Int = DEFAULT_NUM_ZEROS): Int = {
    val hashPrefix = List.fill(numZeros)("0").mkString
    val numbers = Stream.from(from)
    val hashesWithNumbers = numbers.map(number => {
      val hashed = hash(secret + number)
      (hashed, number)
    })
    val hashWithCoin = hashesWithNumbers.find(hashAndNumber => {
      val (hashed, _) = hashAndNumber
      hashed.startsWith(hashPrefix)
    })
    hashWithCoin.get._2
  }

  def hash(str: String): String = {
    digester.digest(str.getBytes()).map("%02x".format(_)).mkString
  }


  private val DEFAULT_FROM = 1
  private val DEFAULT_NUM_ZEROS = 5
  private val HEX_RADIX = 16
  private def digester =  MessageDigest.getInstance("MD5")
}
