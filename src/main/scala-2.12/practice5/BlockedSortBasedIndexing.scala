package practice5

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.util.concurrent.ConcurrentLinkedDeque

import common._

import scala.io.BufferedSource
import scala.io.Source
import scalaz._
import Scalaz._
import scala.collection.SortedMap


object BlockedSortBasedIndexing {

  private val MAX_BLOCK_SIZE: Long = (1.toLong << 30) / Runtime.getRuntime.availableProcessors()

  def processCollection(documentCollection: DocumentCollection) : Unit = {


    val partCounts : Seq[(String, Int)] = generatePartialIndice(documentCollection)

    val parts : Seq[String] = partCounts.flatMap(x => (1 to x._2).map(i => s"${x._1}.partial$i"))

    parts.reduceLeft {
      case (filename1, filename2) =>
        mergePartials(filename1, filename2)
        filename1
    }
  }

  private def generatePartialIndice(documentCollection: DocumentCollection) : Seq[(String, Int)] = {
      documentCollection.getPaths.par.map(doc => {
          val source = documentCollection.getSource(doc)
          val partsCount = DList.unfoldr[Unit, (Iterator[String], Int)](source.getLines().map(_.split("[^\\w]+")).flatten -> 1, {
            case (iter, i) =>
              if(iter.nonEmpty) {
                iter.zipWithIndex.span(_._2 < MAX_BLOCK_SIZE).bimap(_.map(_._1), _.map(_._1)) match {
                  case (l, rest) =>
                    val indexedL = l.toIndexedSeq
                    val revertIndex = CoordinateRevertIndex.fromTokens(indexedL)
                    revertIndex.toCollectionIndex(doc).saveToFile(s"$doc.partial$i")

                    Some(() -> (rest, i + 1))
                }
              } else {
                None
              }
          }).length

          doc -> partsCount
      }).seq
  }

  private def mergePartials(filename1 : String, filename2 : String) = {
    val reader1 = new BufferedReader(new FileReader(filename1))
    val reader2 = new BufferedReader(new FileReader(filename2))

    val writer = new BufferedWriter(new FileWriter("tmp"))

    def _mergePartials() : Unit =
      (readNextWordPos(reader1), readNextWordPos(reader2)) match {
        case (None, None) => ()
        case (Some((word, col)), None) =>
          writeWordPos(writer, word, col)
          _mergePartials()
        case (None, Some((word, col))) =>
          writeWordPos(writer, word, col)
          _mergePartials()
        case (Some((word, col1)), Some((word2, col2))) if word == word2 =>
          writeWordPos(writer, word, col1.merge(col2))
          _mergePartials()
      }

    reader1.close()
    reader2.close()
    writer.flush()
    writer.close()

    println(s"Merged $filename1 with $filename2")

    Files.copy(Paths.get("tmp"), Paths.get(filename1), StandardCopyOption.REPLACE_EXISTING)
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

  def writeWordPos(writer : BufferedWriter, word : String, positions : CollectionWordPositions) : Unit = {
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
}
