package common

import scala.collection.immutable.BitSet

trait IncidenceMatrix {
  val words : Vector[String]
  val documents : Vector[String]
  val matrix : Vector[Vector[Boolean]]
}

object IncidenceMatrix {
  def fromReverseIndex(collectionRevertIndex: CollectionRevertIndex) : IncidenceMatrix = {
    val allWords : Iterable[String] = collectionRevertIndex.positionsByWord.keys
    val allDocuments : Set[String] = collectionRevertIndex.positionsByWord.map(_._2.positionsByDocument.keySet).reduce(_ ++ _)

    val incidenceMatrix =
      allWords.map(word =>
        allDocuments.map(doc => collectionRevertIndex.positionsByWord(word).positionsByDocument.contains(doc)).toVector
      ).toVector

    new IncidenceMatrix {
      override val words: Vector[String] = allWords.toVector
      override val documents: Vector[String] = allDocuments.toVector
      override val matrix: Vector[Vector[Boolean]] = incidenceMatrix
    }
  }
}
