package practice7

import scala.io.{BufferedSource, Source}
import Aliases._

/**
  * Created by alosha on 3/19/17.
  */
trait ZonedDocument {
  val name : DocumentName
  val zoneSources : Map[ZoneName, Source]
}
