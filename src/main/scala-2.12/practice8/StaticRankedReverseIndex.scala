package practice8

import Aliases._
import scalaz._, Scalaz._

import scala.collection.immutable.SortedSet

/**
  * Created by alosha on 3/26/17.
  */
class StaticRankedReverseIndex(
        val data : Map[Term, SortedSet[StaticRankedDocument]] = Map.empty, // SortedSet is sorted by static rank descending
        val allDocuments : Vector[StaticRankedDocument] = Vector.empty
) {

    def + : StaticRankedDocument => StaticRankedReverseIndex = {
      case document : StaticRankedDocument =>
        new StaticRankedReverseIndex(
          document.termSet.foldLeft(this.data) {
            case (revIndexData, term) =>
              revIndexData.updated(term, revIndexData.getOrElse(term, SortedSet.empty[StaticRankedDocument]) + document)
          },
          allDocuments :+ document
        )
    }

    private lazy val leaderToClosestClusterMap : Map[StaticRankedDocument, Term] = {
      (1 to Math.sqrt(allDocuments.size).toInt)
        .map(_ => Math.random())
        .toList
        .mapAccumL[Vector[StaticRankedDocument], StaticRankedDocument](allDocuments) {
          case (docs: Vector[StaticRankedDocument], rnd: Double) =>
            val i = (rnd * docs.size).toInt
            docs.splitAt(i) match {
              case (l, r) => (l ++ r, docs(i))
            }
        }._2
        .map {
          leader: StaticRankedDocument => leader -> data.keys.minBy(term => data(term).head.cosineSimilarity(leader))
        }.toMap
    }

    private lazy val leaders : Iterator[StaticRankedDocument] = leaderToClosestClusterMap.keysIterator

    def getSortedResultsByTerms(terms : Set[Term]) : Seq[StaticRankedDocument] = {
      val leader : StaticRankedDocument = leaders.minBy(_.cosineSimilarity(terms))
      val clusterName : Term = leaderToClosestClusterMap(leader)
      data(clusterName).toSeq.sortBy(_.cosineSimilarity(terms))
    }
}

object StaticRankedReverseIndex {
  def fromCollection(staticRankedCollection: StaticRankedCollection) = {
    staticRankedCollection.documents.foldLeft(new StaticRankedReverseIndex) {
      case (revIndex, doc) =>
        revIndex + doc
    }
  }
}
