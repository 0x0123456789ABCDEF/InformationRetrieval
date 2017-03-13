package practice6

import scala.collection.mutable

/**
  * Created by alosha on 3/13/17.
  */
class ZippedWordCollection {
    private val allWords : mutable.StringBuilder = new StringBuilder()
    private var sortedWordStarts : Vector[Int] = Vector()

    def getId(word : String) : Int = {
      bSearch(sortedWordStarts.zipWithIndex, word) match {
        case Some(id) => id
        case None =>
          this.synchronized {
            val wordStart = allWords.length
            allWords append word
            sortedWordStarts = sortedWordStarts :+ wordStart
            sortedWordStarts.length - 1
          }
      }
    }

    private def bSearch(indexedWordStarts : Vector[(Int, Int)], word : String) : Option[Int] = indexedWordStarts match {
      case Vector() => None
      case Vector((i, wordStart)) =>
          Some(i).filter(_ => getWord(wordStart, word.length).equals(word))
      case Vector(x, y) => bSearch(Vector(x), word).orElse(bSearch(Vector(y), word))
      case vec =>
        val (l, (i, wordStart) +: r) = vec.splitAt(vec.length/2)
        getWord(wordStart, word.length) match {
          case s if s.equals(word) => Some(i)
          case s if s < word => bSearch(r, word)
          case s if s > word => bSearch(l, word)
        }
    }

    private def getWord(wordStart : Int, wordLength : Int) : String =
      allWords.substring(wordStart, wordLength)
}
