package practice7

import Aliases._
import common.BufferedSourceTokenizer

/**
  * Created by alosha on 3/19/17.
  */
class ZonedReverseIndex(
  val zonePriorities : Map[ZoneName, ZonePriority]
) {
  val data : Map[Term, Map[DocumentName, Set[ZoneName]]] = Map.empty

  def + : ((Term, DocumentName, ZoneName)) => ZonedReverseIndex = {
    case (word, docName, zoneName) =>
      val docMap = data.getOrElse(word, Map.empty)
      val zoneSet = docMap.getOrElse(docName, Set.empty)
      val updatedZoneSet = zoneSet + zoneName
      val updatedDocMap = docMap.updated(docName, updatedZoneSet)
      val updatedData = data.updated(word, updatedDocMap)
      new ZonedReverseIndex(zonePriorities) {
        override val data = updatedData
      }
  }

  def getBestDocumentsByTerm(term : Term, numberOfDocuments : Int) : Seq[(DocumentName, DocumentPriority)] = {
    data(term).toSeq.map {
      case (docName, zoneSet) =>
        (docName, DocumentPriority(zoneSet.map(zonePriorities).map(_.priority).sum))
    }.sortBy(-_._2.priority).take(numberOfDocuments)
  }

  def getBestDocumentsByTerms(terms : Seq[Term], numberOfDocuments : Int) : Seq[(DocumentName, DocumentPriority)] = {
    val bestPartialResults : Seq[Map[DocumentName, DocumentPriority]] = terms.map(getBestDocumentsByTerm(_, numberOfDocuments * 10).toMap)

    val bestResults : Option[Map[DocumentName, DocumentPriority]] =
      bestPartialResults.reduceOption[Map[DocumentName, DocumentPriority]] {
        case (priorityMap1, priorityMap2) =>
          val allDocs = priorityMap1.keySet ++ priorityMap2.keySet
          allDocs.map(doc => {
            val newPriority = priorityMap1.getOrElse(doc, DocumentPriority(0)) + priorityMap2.getOrElse(doc, DocumentPriority(0))
            (doc, newPriority)
          }).toMap
      }

    bestResults
      .getOrElse(Map[DocumentName, DocumentPriority]())
      .toSeq
      .sortBy(-_._2.priority)
      .take(numberOfDocuments)
  }
}

object ZonedReverseIndex {
  def fromCollection(zonedCollection: ZonedCollection) : ZonedReverseIndex = {
    zonedCollection.documents.flatMap {
      document: ZonedDocument =>
        document.zoneSources.flatMap {
          case (zoneName, bufSource) =>
            val terms: Seq[Term] = BufferedSourceTokenizer(bufSource).map(Term)
            terms.map(zoneName -> _)
        } map {
          case (zoneName, term) =>
            (term, document.name, zoneName)
        }
    }.foldLeft[ZonedReverseIndex](new ZonedReverseIndex(zonedCollection.zonePriorities)) {
      case (indexSoFar : ZonedReverseIndex, indexItem : (Term, DocumentName, ZoneName)) =>
        indexSoFar + indexItem
    }
  }
}