package practice6

import common.CollectionWordPositions

import scala.collection.mutable

/**
  * Created by alosha on 3/13/17.
  */
class ZippedIndex {
  val zippedWords = new ZippedWordCollection
  val zippedDocNames = new ZippedWordCollection

  type WordId = Int
  type DocId = Int
  type DocPositions = Seq[Int]

  val wordPositions : mutable.Map[WordId, Map[DocId, DocPositions]] = mutable.HashMap()


  def += : (String, CollectionWordPositions) => Unit = {
    case (word, wordPos) =>
      val wordId = zippedWords.getId(word)
      val innerMap : mutable.Map[DocId, DocPositions] = mutable.HashMap()
      wordPos.positionsByDocument.map({
        case (docName : String, docPositions : Seq[Int]) =>
          val docId = zippedDocNames.getId(docName)
          innerMap.put(docId, docPositions)
      })
      wordPositions.put(wordId, Map(innerMap.toSeq : _*))
  }
}
