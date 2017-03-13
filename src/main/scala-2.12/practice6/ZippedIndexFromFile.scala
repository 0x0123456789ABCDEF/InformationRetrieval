package practice6

import java.io.{BufferedReader, FileInputStream, FileReader, InputStreamReader}

import common.CollectionWordPositions

/**
  * Created by alosha on 3/13/17.
  */
object ZippedIndexFromFile {
  def apply(filename : String) : ZippedIndex = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(filename)))
    val zippedIndex = new ZippedIndex
    Stream.continually(readNextWordPos(reader)).takeWhile(_.isDefined).map(_.get).foreach {
      case (word, wordPos) => zippedIndex += (word, wordPos)
    }
    zippedIndex
  }

  private def readNextWordPos(reader : BufferedReader) : Option[(String, CollectionWordPositions)] = {
    val word = reader.readLine()
    if(word == null)
      None
    else {
      val docPositions = new CollectionWordPositions {
        val positionsByDocument: Map[String, Seq[Int]] =
          readLine().split(";").filter(!_.isEmpty).map(_.split(",").toList match {
            case doc :: pos =>
              doc -> pos.map(_.toInt).toSeq
            case Nil =>
              "" -> null
          }).toMap
      }
      Some(word -> docPositions)
    }
  }
}
