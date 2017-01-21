package practice1

import java.io.PrintWriter

import scala.io.Source
import scalaz._
import Scalaz._
import scala.collection.immutable.SortedMap

/**
  * @author alosha
  */
object Practice1 extends App {

      val bookNames = (1 to 10).map("practice1/book" + _ + ".txt")

      val reverseIndex = SortedMap(
        bookNames.par.map(bookName => {
          Source.fromResource(bookName).getLines().zipWithIndex.flatMap({
            case (line, lineIndex) => line.split("[^\\w]+").filter(!_.isEmpty).map(_.toLowerCase -> lineIndex)
          }).toStream.groupBy(_._1).mapValues(_.map(bookName -> _._2))
        }).reduce(_ |+| _).toSeq : _*
      )

      val formatInt = java.text.NumberFormat.getIntegerInstance.format(_ : Long)

      val reverseIndexPrettyPrint =
        reverseIndex.mapValues(_.sorted.map({ case (doc, line) => s"Document $doc; line $line\n" }).mkString).map({
          case (word, result) =>
            s"""----- WORD '$word' -----
                | Occurances: ${formatInt(reverseIndex(word).size)}
                |
                |$result
                |
                |""". stripMargin
        }).mkString


      val result =
        s"""
           | Total character count: ${formatInt(bookNames.par.map(Source.fromResource(_)).map(_.iter.size.toLong).sum)}
           | Total words count: ${formatInt(reverseIndex.map(_._2.size).sum)}
           | Unique words count: ${formatInt(reverseIndex.size)}
           |
           | Reverse index:
           |
           | $reverseIndexPrettyPrint
           |
         """.stripMargin

      new PrintWriter("output_1.txt") { write(result); close() }
}