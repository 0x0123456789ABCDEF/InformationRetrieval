package common

trait IncidenceMatrix {
  val words : IndexedSeq[String]
  val documents : IndexedSeq[String]
  val matrix : IndexedSeq[IndexedSeq[Boolean]]
}

object IncidenceMatrix {
  def fromReverseIndex(collectionRevertIndex: CollectionRevertIndex) : IncidenceMatrix = {
    val allWords : Iterable[String] = collectionRevertIndex.positionsByWord.keys
    val allDocuments : Set[String] = collectionRevertIndex.positionsByWord.map(_._2.positionsByDocument.keySet).reduce(_ ++ _)

    val incidenceMatrix =
      allWords.map(word =>
        allDocuments.map(doc => collectionRevertIndex.positionsByWord(word).positionsByDocument.contains(doc)).toIndexedSeq
      ).toIndexedSeq

    new IncidenceMatrix {
      override val words: IndexedSeq[String] = allWords.toIndexedSeq
      override val documents: IndexedSeq[String] = allDocuments.toIndexedSeq
      override val matrix: IndexedSeq[IndexedSeq[Boolean]] = incidenceMatrix
    }
  }
}
