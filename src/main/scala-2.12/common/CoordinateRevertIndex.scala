package common

import java.io.{BufferedWriter, File, FileWriter}

trait DocumentCoordinateRevertIndex {
  val positionsByWord : Map[String, Seq[Int]]
  def toCollectionIndex(docName : String) : CollectionCoordinateRevertIndex = {
    val thiz = this
    new CollectionCoordinateRevertIndex {
      override val positionsByWord: Map[String, CollectionWordPositions] =
        thiz.positionsByWord.map{
          case (word, docPositions) => (word, new CollectionWordPositions {
            override val positionsByDocument: Map[String, Seq[Int]] = Map(docName -> docPositions)
          })
        }
    }
  }
}

trait CollectionCoordinateRevertIndex {
  val positionsByWord : Map[String, CollectionWordPositions]

  def saveToFile(filename : String) = {
    println(s"Saving $filename...")

    val writer = new BufferedWriter(new FileWriter(filename))

    positionsByWord.toIndexedSeq.sortBy(_._1).foreach {
      case (word, positions) =>
        writer.write(word)
        writer.write('\n')
        positions.positionsByDocument.foreach {
          case (document, positions) =>
            writer.write(document)
            positions.foreach(i => { writer.write(','); writer.write(i.toString)})
            writer.write(';')
        }
        writer.write('\n')
    }

    writer.flush()
    writer.close()

    println(s"$filename saved! Size: ${new File(filename).length() / (1024 * 1024).toDouble} MB")
  }
}

trait CollectionWordPositions {
  val positionsByDocument : Map[String, Seq[Int]]

  def merge(pos : CollectionWordPositions) : CollectionWordPositions =
    new CollectionWordPositions {
      override val positionsByDocument: Map[String, Seq[Int]] =
        (positionsByDocument.keySet ++ pos.positionsByDocument.keySet).map(doc => {
          doc -> (positionsByDocument.getOrElse(doc, Seq()) ++ pos.positionsByDocument.getOrElse(doc, Seq()))
        }).toMap
    }
}

object CoordinateRevertIndex {
  def fromCollection(collection: DocumentCollection): CollectionCoordinateRevertIndex = {
    def fromDocument(documentName: String): DocumentCoordinateRevertIndex =
      fromTokens(BufferedSourceTokenizer(collection.getSource(documentName)))
    new CollectionCoordinateRevertIndex {
      override val positionsByWord: Map[String, CollectionWordPositions] =
        collection.getPaths
          .map(documentName => documentName -> fromDocument(documentName))
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
  }



  def fromTokens(tokens: Seq[String]): DocumentCoordinateRevertIndex =
    new DocumentCoordinateRevertIndex {
      override val positionsByWord: Map[String, Seq[Int]] =
        tokens.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))
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
