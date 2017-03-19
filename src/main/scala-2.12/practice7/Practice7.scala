package practice7

import Aliases._

import scala.io.{BufferedSource, Source, StdIn}

/**
  * Created by alosha on 3/19/17.
  */
object Practice7 extends App {
  val testCollection = new ZonedCollection {
    override def zonePriorities = Map(
      ZoneName("title") -> ZonePriority(0.2),
      ZoneName("author") -> ZonePriority(0.3),
      ZoneName("body") -> ZonePriority(0.5)
    )
    override def documents = Seq(
      new ZonedDocument {
        override val name = DocumentName("doc1")
        override val zoneSources: Map[ZoneName, BufferedSource] = Map(
          ZoneName("title") -> Source.fromFile("doc1.title"),
          ZoneName("author") -> Source.fromFile("doc1.author"),
          ZoneName("body") -> Source.fromFile("doc1.body")
        )
      },
      new ZonedDocument {
        override val name: DocumentName = DocumentName("doc2")
        override val zoneSources: Map[ZoneName, BufferedSource] = Map(
          ZoneName("title") -> Source.fromFile("doc2.title"),
          ZoneName("author") -> Source.fromFile("doc2.author"),
          ZoneName("body") -> Source.fromFile("doc2.body")
        )
      },
      new ZonedDocument {
        override val name: DocumentName = DocumentName("doc3")
        override val zoneSources: Map[ZoneName, BufferedSource] = Map(
          ZoneName("title") -> Source.fromFile("doc3.title"),
          ZoneName("author") -> Source.fromFile("doc3.author"),
          ZoneName("body") -> Source.fromFile("doc3.body")
        )
      }
    )
  }

  val index = ZonedReverseIndex.fromCollection(testCollection)

  while(true) {
    val query : String = StdIn.readLine()
    val terms : Seq[Term] = query.split("[\w+]").map(_.toLowerCase).map(Term)
    val result : Seq[(DocumentName, DocumentPriority)] = index.getBestDocumentsByTerms(terms, numberOfDocuments = 10)
    result.foreach {
      case (DocumentName(docName), DocumentPriority(docPriority)) =>
        println(s"$docName (priority - $docPriority)")
    }
  }
}
