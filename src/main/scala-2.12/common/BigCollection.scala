package common
import scala.io.{BufferedSource, Source}

object BigCollection extends DocumentCollection {
  def getPaths : Seq[String] =
    (1 to 10).map("bigdoc" + _ + ".txt")

  override def getSource(path: String): BufferedSource =
    Source.fromFile(path)
}
