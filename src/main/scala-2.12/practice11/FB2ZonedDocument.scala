package practice11
import practice7.Aliases.{DocumentName, ZoneName, ZonePriority}
import practice7.{ZonedCollection, ZonedDocument}

import scala.io.{BufferedSource, Source}
import scala.xml.{Elem, XML}

/**
  * Created by alosha on 4/9/17.
  */
object FB2ZonedDocument {
  def fromFile(filename : String) : ZonedDocument = {
    val doc = XML.loadFile(filename)
    new ZonedDocument {
      override val name: DocumentName = DocumentName(filename)
      override val zoneSources: Map[ZoneName, Source] = Map(
        ZoneName("author") -> Source.fromString((doc \\ "author").map(_.text).mkString(" ")),
        ZoneName("keywords") -> Source.fromString((doc \\ "keywords").map(_.text).mkString(" ")),
        ZoneName("date") -> Source.fromString((doc \\ "date").map(_.text).mkString(" ")),
        ZoneName("title") -> Source.fromString(((doc \\ "book-title") ++ (doc \\ "book-name")).map(_.text).mkString(" ")),
        ZoneName("body") -> Source.fromString((doc \\ "body").map(_.text).mkString(" "))
      )
    }
  }
}


