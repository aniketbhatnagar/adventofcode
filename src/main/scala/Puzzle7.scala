import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 7: Some Assembly Required ---
 *
 * http://adventofcode.com/day/7
 *
 * @author aniket
 */
object Puzzle7 {

  def parseCircuit(instructions: String): Circuit = {
    val parser = new InstructionParsers()
    parser.parse(instructions)
  }

  sealed trait Gate {
    def value: Char
  }

  sealed trait Source {
    def value: Char // Char is unsigned 16-bit
  }

  case class Wire(val name: String, var sourceOpt: Option[Source]) {
    private var valueCache: Option[Char] = None

    def value: Char = {
      if (valueCache.isEmpty) {
        val computedVal = sourceOpt.map(source => source.value).getOrElse(throw new IllegalStateException(s"No source for $name"))
        valueCache = Some(computedVal)
      }
      valueCache.get
    }

    def resetCache(): Unit = {
      valueCache = None
    }
  }

  case class ValueSource(val value: Char) extends Source

  case class WireSource(wire: Wire) extends Source {
    override def value: Char = wire.value
  }
  case class GateSource(gate: Gate) extends Source {
    override def value: Char = gate.value
  }

  case class Not(inputSource: Source) extends Gate {
    override def value: Char = (~inputSource.value).toChar
  }

  case class LShift(inputSource: Source, val noBits: Short) extends Gate {
    override def value: Char = (inputSource.value << noBits).toChar
  }

  case class RShift(inputSource: Source, val noBits: Short) extends Gate {
    override def value: Char = (inputSource.value >> noBits).toChar
  }

  case class And(input1: Source, input2: Source) extends Gate {
    override def value: Char = (input1.value & input2.value).toChar
  }

  case class Or(input1: Source, input2: Source) extends Gate {
    override def value: Char = (input1.value | input2.value).toChar
  }

  class Circuit(wires: List[Wire]) {
    private val wiresLookup = wires.map(wire => (wire.name, wire)).toMap

    def valueOf(wireName: String): Char = wiresLookup(wireName).value

    def overrideSource(wireName: String, source: Source): Unit = {
      wiresLookup(wireName).sourceOpt = Some(source)
      resetCache()
    }

    private def resetCache(): Unit = {
      for (wire <- wires) {
        wire.resetCache()
      }
    }
  }

  class InstructionParsers extends JavaTokenParsers {

    object ParsingContext {
      private val wires = mutable.HashMap[String, Wire]()

      def getOrRegister(wireName: String): Wire = {
        wires.getOrElseUpdate(wireName, new Wire(wireName, None))
      }
    }

    private val context = ParsingContext

    private val AND: Parser[String] = "AND"
    private val OR: Parser[String] = "OR"
    private val NOT: Parser[String] = "NOT"
    private val LSHIFT: Parser[String] = "LSHIFT"
    private val RSHIFT: Parser[String] = "RSHIFT"
    private val PROVIDE: Parser[String] = "->"
    private val value: Parser[Char] = wholeNumber map(_.toInt.toChar)
    private val wireName: Parser[String] = ident


    def wireSource: Parser[Source] = wireName.map(name => new WireSource(context.getOrRegister(name)))
    def valueSource: Parser[Source] = value.map(value => new ValueSource(value))
    def inputSource: Parser[Source] =  wireSource | valueSource
    def notGate: Parser[Gate] = (NOT ~> inputSource).map(matchTuple => new Not(matchTuple))
    def andGate: Parser[Gate] = ((inputSource <~ AND) ~ inputSource).map(matchTuple => {
      new And(matchTuple._1, matchTuple._2)
    })
    def orGate: Parser[Gate] = ((inputSource <~ OR) ~ inputSource).map(matchTuple => {
      new Or(matchTuple._1, matchTuple._2)
    })
    def lshiftGate: Parser[Gate] = ((inputSource <~ LSHIFT) ~ value).map(matchTuple => {
      new LShift(matchTuple._1, matchTuple._2.toShort)
    })
    def rshiftGate: Parser[Gate] = ((inputSource <~ RSHIFT) ~ value).map(matchTuple => {
      new RShift(matchTuple._1, matchTuple._2.toShort)
    })
    def gateSource: Parser[Source] = (notGate | andGate | orGate | lshiftGate | rshiftGate).map(gate => new GateSource(gate))

    def source: Parser[Source] = gateSource | inputSource

    def expr: Parser[Wire] = ((source <~ PROVIDE) ~ wireName).map(matchTuple => {
      val wire = context.getOrRegister(matchTuple._2)
      wire.sourceOpt = Some(matchTuple._1)
      wire
    })
    def exprs: Parser[Circuit] = (expr*).map(wires => new Circuit(wires))

    def parse(instructions: String): Circuit = {
      val input = new CharSequenceReader(instructions)
      val parserResult = exprs(input)
      val circuit: Circuit = parserResult match {
        case Success(circuit, _) => circuit
        case NoSuccess(msg, _)   => throw new IllegalArgumentException(s"Parsing failed: $msg")
      }
      circuit
    }
  }

}
