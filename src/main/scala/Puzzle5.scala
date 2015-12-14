/**
 * --- Day 5: Doesn't He Have Intern-Elves For This? ---
 *
 * http://adventofcode.com/day/5
 *
 * @author aniket
 */
object Puzzle5 {


  trait PuzzleSolver {

    sealed trait Error {
      def msg: String
    }

    def validators: List[String => Either[Error, Unit]]

    def countNice(strings: List[String]): Int = {
      strings.filter(validateNice(_).isRight).size
    }

    def validateNice(str: String): Either[Error, Unit] = {
      validate(str, validators)
    }

    private def validate(str: String, validators: List[String => Either[Error, Unit]]): Either[Error, Unit] = {
      validators.foldLeft[Either[Error, Unit]](Right[Error, Unit]())((oldRes, validator) => {
        oldRes.right.flatMap(_ => validator(str))
      })
    }
  }

  object Part1Solver extends PuzzleSolver {

    override val validators: List[String => Either[Error, Unit]] = List(
      validateBlacklisted,
      validateVowels,
      validateCharsInRow)

    private def validateCharsInRow(str: String): Either[Error, Unit] = {
      val initialResult: Either[Error, Unit] = Left(LetterNotRepeatedInARow)
      val (_, result) = str.tail.foldLeft((str.head, initialResult))((prevCharAndResult, char) => {
        val (prevChar, result) = prevCharAndResult
        if (prevChar.equals(char)) {
          (char, Right())
        } else {
          (char, result)
        }
      })
      result
    }

    private def validateVowels(str: String): Either[Error, Unit] = {
      val vowelCounts = buildCharCounts(str.filter(char => "aeiou".contains(char)))
      val vowelCount = vowelCounts.values.sum
      if (vowelCount >= 3) {
        Right()
      } else {
        Left(ContainsLessThan3Vowels)
      }
    }

    private def validateBlacklisted(str: String): Either[Error, Unit] = {
      if (!str.matches(BLACKLISTED_STRINGS_REGEX)) {
        Right()
      } else {
        Left(ContainsBlacklisted)
      }
    }

    private def buildCharCounts(str: String): Map[Char, Int] = {
      str.groupBy(ch => ch).map {
        case (ch, matchedStr) => (ch, matchedStr.size)
      }
    }


    case object ContainsBlacklisted extends Error {
      override val msg: String = "Contains ab or cd or pq or xy"
    }

    case object ContainsLessThan3Vowels extends Error {
      override val msg: String = "Contains less than 3 vowels"
    }

    case object LetterNotRepeatedInARow extends Error {
      override val msg: String = "No letter is repeated in a row"
    }

    private val BLACKLISTED_STRINGS_REGEX = ".*(ab|cd|pq|xy).*"
  }

  object Part2Solver extends PuzzleSolver {
    override def validators: List[(String) => Either[Part2Solver.Error, Unit]] = List(
      validateLetterRepeatsAfterOneLetter,
      validatePairRepeats
    )

    def validatePairRepeats(str: String): Either[Error, Unit] = {
      val strWithIndex = str.zip(0 until str.size)
      val initialResult: Either[Error, Unit] = Left(PairDoesNotRepeat)
      val finalState = strWithIndex.tail.foldLeft((str.head, Map[String, Int](), initialResult))((state, charWithIndex) => {
        var (char, index) = charWithIndex
        val (lastChar, pairsSeen, result) = state
        val currentPair = new String(Array(lastChar, char))
        if (pairsSeen.contains(currentPair)) {
          val pairIndex = pairsSeen(currentPair)
          if (index - pairIndex > 2) {
            (char, pairsSeen.updated(currentPair, index - 1), Right())
          } else {
            (char, pairsSeen, result)
          }
        } else {
          (char, pairsSeen.updated(currentPair, index - 1), result)
        }
      })
      finalState._3
    }

    def validateLetterRepeatsAfterOneLetter(str: String): Either[Error, Unit] = {
      if (str.size < 2) {
        Left(LetterDoesntRepeatAfterOneLetter)
      } else {
        val initialResult: Either[Error, Unit] = Left(LetterDoesntRepeatAfterOneLetter)
        val finalState = str.drop(2).foldLeft((str(0), str(1), initialResult))((state, char) => {
          val (secondLastChar, lastChar, result) = state
          if (secondLastChar.equals(char)) {
            (lastChar, char, Right())
          } else {
            (lastChar, char, result)
          }
        })
        finalState._3
      }
    }

    case object LetterDoesntRepeatAfterOneLetter extends Error {
      override val msg: String = "Doesn't contain at least one letter which repeats with exactly one letter between them"
    }

    case object PairDoesNotRepeat extends Error {
      override val msg: String = "no pair that appears twice"
    }

  }
}
