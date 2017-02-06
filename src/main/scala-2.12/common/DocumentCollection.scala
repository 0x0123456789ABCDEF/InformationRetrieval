package common

import scala.io.{Source, BufferedSource}

abstract class DocumentCollection {
  def getPaths : Seq[String]

  def getSources : Seq[BufferedSource] =
    getPaths.map(Source.fromResource(_))
}
