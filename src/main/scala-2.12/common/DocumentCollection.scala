package common

import scala.io.{Source, BufferedSource}

abstract class DocumentCollection {
  def getPaths : Seq[String]

  def getSource(path: String) : BufferedSource

}
