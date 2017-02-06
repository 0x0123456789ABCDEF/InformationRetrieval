package common

trait DocumentCoordinateRevertIndex {
  val positionsByWord : Map[String, Seq[Int]]
}

trait CollectionCoordinateRevertIndex {
  val positionsByWord : Map[String, CollectionWordPositions]
}

trait CollectionWordPositions {
  val positionsByDocument : Map[String, Seq[Int]]
}

object CoordinateRevertIndex {
  def fromCollection(collection: DocumentCollection): CollectionCoordinateRevertIndex =
    new CollectionCoordinateRevertIndex {
      override val positionsByWord: Map[String, CollectionWordPositions] =
        collection.getPaths
          .map(documentName => documentName -> CoordinateRevertIndex.fromDocument(documentName))
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

  private def fromDocument(documentName: String): DocumentCoordinateRevertIndex =
    new DocumentCoordinateRevertIndex {
      override val positionsByWord: Map[String, Seq[Int]] =
        DocumentTokenizer(documentName).zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))
    }

  def coordinatedIntersection(result1 : Seq[Int], result2 : Seq[Int], maxDistance : Int, accum : Seq[Int] = Seq()): Seq[Int] = {
    (result1, result2) match {
      case (xxs@(x +: xs), yys@(y +: ys)) =>
        if(Math.abs(x - y) <= maxDistance)  {
          coordinatedIntersection(xs, ys, maxDistance, accum ++ Seq(Math.min(x, y), Math.max(x, y)))
        } else {
          if(x < y) {
            coordinatedIntersection(xs, yys, maxDistance, accum)
          } else {
            coordinatedIntersection(xxs, ys, maxDistance, accum)
          }
        }
      case _ => accum
    }
  }
}
