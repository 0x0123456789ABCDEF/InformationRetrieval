package common

import scalaz._


trait DocumentRevertIndex {
  val positionsByWord : Map[String, Seq[Int]]
}

trait CollectionRevertIndex {
  val positionsByWord : Map[String, CollectionWordPositions]
}

trait CollectionWordPositions {
  val positionsByDocument : Map[String, Seq[Int]]
}

object RevertIndex {
  def fromCollection(collection: Collection): CollectionRevertIndex =
    new CollectionRevertIndex {
      override val positionsByWord: Map[String, CollectionWordPositions] =
        collection.getPaths
          .map(documentName => documentName -> RevertIndex.fromDocument(documentName))
          .foldLeft(Map.empty[String, Map[String, Seq[Int]]].withDefaultValue(Map.empty)) {
            case (accum, (documentName, wordRevertIndex)) =>
              wordRevertIndex.positionsByWord.foldLeft(accum) {
                case (accum2, (word, indice)) => accum2.updated(word, accum2(word).updated(documentName, indice))
              }
          }.mapValues(pos =>
            new CollectionWordPositions {
              override val positionsByDocument: Map[String, Seq[Int]] = pos
            }
          )
    }

  private def fromDocument(documentName: String): DocumentRevertIndex =
    new DocumentRevertIndex {
      override val positionsByWord: Map[String, Seq[Int]] =
        DocumentTokenizer(documentName).zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))
    }
}
