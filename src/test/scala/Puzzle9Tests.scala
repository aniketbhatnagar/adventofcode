import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

/**
 * @author aniket
 */
class Puzzle9Tests extends FlatSpec with Matchers {

  import Puzzle9._
  import GraphParsers._

  "Puzzle9" should "solve example in part 1" in {
    val graph = exampleGraph
    val path = graph.shortestPath
    path.totalDistance should be (605)
  }

  it should "solve part 1" in {
    val graph = parse(input)
    val path = graph.shortestPath
    path.vertexIdx.size should be (graph.vertices.size)
    path.totalDistance should be (141)
  }

  it should "solve part 2" in {
    val graph = parse(input)
    val path = graph.longestPath
    path.vertexIdx.size should be (graph.vertices.size)
    path.totalDistance should be (736)
  }

  private def exampleGraph: Graph = {
    parse(
      """London to Dublin = 464
        |London to Belfast = 518
        |Dublin to Belfast = 141
      """.stripMargin)
  }

  private val input: String = {
    val source = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("day9_input"))
    source.mkString
  }
}
