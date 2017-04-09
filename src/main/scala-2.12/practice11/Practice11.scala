package practice11

import practice7.Aliases.{DocumentName, DocumentPriority, Term}
import practice7.ZonedReverseIndex

import scala.io.StdIn

/**
  * Created by alosha on 4/9/17.
  */
class Practice11 extends App {

  val testCollection = FB2ZonedCollection.fromFiles("test.fb2", "test2.fb2", "test3.fb2", "test4.fb2", "test5.fb2")

  val index = ZonedReverseIndex.fromCollection(testCollection)

  while(true) {
    val query : String = StdIn.readLine()
    val terms : Seq[Term] = query.split("[\\w+]").map(_.toLowerCase).map(Term)
    val result : Seq[(DocumentName, DocumentPriority)] = index.getBestDocumentsByTerms(terms, numberOfDocuments = 10)
    result.foreach {
      case (DocumentName(docName), DocumentPriority(docPriority)) =>
        println(s"$docName (priority - $docPriority)")
    }
  }
}
