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
      val reverseIndex = SortedMap(
        (1 to 10).map("practice1/book" + _ + ".txt").par.map(documentName => {
          Source.fromResource(documentName).getLines().zipWithIndex.flatMap({
            case (line, lineIndex) => line.split("[^\\w]+").map(_.toLowerCase -> lineIndex)
          }).toStream.groupBy(_._1).mapValues(_.map(documentName -> _._2))
        }).reduce(_ |+| _).toSeq : _*
      )

      val reverseIndexPrinted =
        reverseIndex.mapValues(_.sorted.map({ case (doc, line) => s"Document $doc; line $line\n" }).mkString).map({
          case (word, result) =>
            s"""----- WORD '$word' -----
                | Occurances: ${reverseIndex(word).size}
                |
                |$result
                |
                |""". stripMargin
        }).mkString

      val result =
        s"""
           | Unique words count: ${reverseIndex.size}
           | Total words count: ${reverseIndex.map(_._2.size).sum}
           |
           | $reverseIndexPrinted
           |
         """.stripMargin

      new PrintWriter("output_1.txt") { write(result); close }
}