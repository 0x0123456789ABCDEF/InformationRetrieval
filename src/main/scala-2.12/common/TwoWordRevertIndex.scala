package common

import scala.collection.mutable.{Map => MMap, Set => MSet}

trait TwoWordRevertIndex {
  val documentsByTwoWords : Map[String, Map[String, Set[String]]]
}


/**
  * Created by alosha on 2/5/17.
  */
object TwoWordRevertIndex {
  def fromCollection(collection: DocumentCollection): TwoWordRevertIndex = {
    var res = MMap.empty[String, MMap[String, MSet[String]]]

    for( (document, words) <- collection.getPaths.map(documentName => documentName -> fromDocument(documentName))) {
      for( (firstWord, secondWords) <- words) {
        if(!res.contains(firstWord)) {
          res.put(firstWord, MMap.empty)
        }
        for(secondWord <- secondWords) {
          if(!res(firstWord).contains(secondWord)) {
            res(firstWord).put(secondWord, MSet())
          }
          res(firstWord)(secondWord) += document
        }
      }
    }

    new TwoWordRevertIndex {
      override val documentsByTwoWords : Map[String, Map[String, Set[String]]] =
        res.mapValues(_.mapValues(_.toSet).toMap).toMap
    }
  }

  private def fromDocument(documentName: String): Map[String, Set[String]] =
        DocumentTokenizer(documentName).sliding(2).toSeq.groupBy(_.head).mapValues(_.map(_.tail.head).toSet)
}
