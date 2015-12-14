/**
 * --- Day 10: Elves Look, Elves Say ---
 *
 * http://adventofcode.com/day/10
 *
 * @author aniket
 */
object Puzzle10 {

  def genSequenceN(input: String, n: Int): String = {
    (1 until n).foldLeft(genSequence(input))((lastSeq, i) => {
      genSequence(lastSeq)
    })
  }

  def genSequence(input: String): String = {
    val finalState = input.tail.foldLeft(("", input.head, 1))((state, char) => {
      val (result, lastChar, lastCharCount) = state
      if (lastChar.equals(char)) {
        (result, lastChar, lastCharCount + 1)
      } else {
        (result + s"$lastCharCount$lastChar", char, 1)
      }
    })
    val (result, lastChar, lastCharCount) = finalState
    result + s"$lastCharCount$lastChar"
  }

}
