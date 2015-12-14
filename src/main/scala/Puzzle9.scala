import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharSequenceReader

/**
 * --- Day 9: All in a Single Night ---
 *
 * http://adventofcode.com/day/9
 *
 * @author aniket
 */
object Puzzle9 {
  case class Edge(source: String, destination: String, distance: Int)
  case class Vertex(val name: String, val outEdges: List[Edge]) {
    override def equals(other: Any): Boolean = {
      other match {
        case otherVertex: Vertex => this.name.equals(otherVertex.name)
        case _                   => false
      }
    }

    override def hashCode(): Int = name.hashCode()
  }
  case class Path(edges: List[Edge]) {
    val vertexIdx = edges.flatMap(edge => List(edge.source, edge.destination)).toSet

    lazy val totalDistance: Int = edges.map(_.distance).sum

    def contains(vertexName: String): Boolean = {
      vertexIdx.contains(vertexName)
    }

    def addEdge(edge: Edge): Path = {
      new Path(edges :+ edge)
    }
  }
  class Graph(edges: List[Edge]) {
    import Graph._
    val vertexEdges: Map[String, List[Edge]] = merge(edges.groupBy(edge => edge.source),
      (edges.map(edge => Edge(edge.destination, edge.source, edge.distance)).groupBy(edge => edge.source)))
    val vertices: List[Vertex] = (vertexEdges.map {
      case (vertexName, vertexEdges) => new Vertex(vertexName, vertexEdges)
    }).toList
    val verticesIdx = vertices.groupBy(_.name).map {
      case (vertexName, vertices) => {
        require(vertices.size == 1)
        (vertexName, vertices(0))
      }
    }

    val allPaths: List[Path] = {
      vertices.permutations.map(vertices => buildPath(vertices)).toList
    }

    val shortestPath: Path = {
      findMinPath(allPaths)
    }

    val longestPath: Path = {
      findMaxPath(allPaths)
    }
  }

  object Graph {
    def findMinEdge(edges: List[Edge]): List[Edge] = {
      val minEdgeDistance = edges.tail.foldLeft(edges.head.distance)((minEdgeDistance, edge) => {
        if (minEdgeDistance > edge.distance) {
          edge.distance
        } else {
          minEdgeDistance
        }
      })
      edges.filter(edge => edge.distance.equals(minEdgeDistance))
    }

    def findMinPath(paths: List[Path]): Path = {
      paths.tail.foldLeft(paths.head)((minPath, path) => {
        if (minPath.totalDistance > path.totalDistance) {
          path
        } else {
          minPath
        }
      })
    }

    def findMaxPath(paths: List[Path]): Path = {
      paths.tail.foldLeft(paths.head)((maxPath, path) => {
        if (maxPath.totalDistance < path.totalDistance) {
          path
        } else {
          maxPath
        }
      })
    }

    def buildPath(vertices: List[Vertex]): Path = {
      val vertexPairs = vertices.dropRight(1) zip vertices.drop(1)
      val edges = vertexPairs.map {
        case (vertex1, vertex2) => vertex1.outEdges.find(edge => edge.destination.equals(vertex2.name)).get
      }
      Path(edges)
    }

    def merge[K, V](map1: Map[K, List[V]], map2: Map[K, List[V]]): Map[K, List[V]] = {
      (map1.toList ++ map2.toList).groupBy(_._1).map {
        case (k, v) => (k, v.flatMap(kv => kv._2))
      }
    }
  }

  object GraphParsers extends JavaTokenParsers {
    def cityName: Parser[String] = ident
    def edge: Parser[Edge] = (((cityName <~ "to") ~ cityName) ~ ("=" ~> wholeNumber)).map(matches => {
      Edge(matches._1._1, matches._1._2, matches._2.toInt)
    })
    def graph: Parser[Graph] = (edge*).map(edges => new Graph(edges))

    def parse(graphStr: String): Graph = {
      val phraseParser = phrase(graph)
      val input = new CharSequenceReader(graphStr)
      phraseParser(input) match {
        case Success(graph, _) => graph
        case NoSuccess(msg, _) => {
          throw new IllegalArgumentException(s"Error parsing string $graphStr: $msg")
        }
      }
    }
  }
}
