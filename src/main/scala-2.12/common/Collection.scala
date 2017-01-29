package common

import scala.io.{Source, BufferedSource}

abstract class Collection {
  def getPaths : Seq[String]

  def getSources : Seq[BufferedSource] =
    getPaths.map(Source.fromResource(_))
}
