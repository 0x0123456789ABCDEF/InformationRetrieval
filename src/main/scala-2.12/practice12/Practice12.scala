package practice12

import common.{BookCollection, CoordinateRevertIndex, SourceTokenizer, TwoWordRevertIndex}

import scala.collection.SortedMap
import scala.io.{Source, StdIn}

object Practice12 extends App {
  val coordinateReverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)
  val twoWordReverseIndex = TwoWordRevertIndex.fromCollection(BookCollection)

  while(true) {
    print("Please type your multiword query: ")

    val query = StdIn.readLine

    val allDocuments = BookCollection.getPaths.toSet

    val docLengths : Map[String, Int] = allDocuments.map(docName => docName -> SourceTokenizer(Source.fromFile(docName)).length).toMap

    val avgDocumentLength = docLengths.values.sum / allDocuments.size

    val queryWords = query.split("\\s+")

    if(queryWords.size > 1) {
      val twoWordIndexResult: Set[String] = queryWords.sliding(2).foldLeft(allDocuments) {
        case (documentsSoFar, wordPair) =>
          val firstWord = wordPair.head
          val secondWord = wordPair.tail.head
          val matchingDocuments = twoWordReverseIndex.documentsByTwoWords.getOrElse(firstWord, Map.empty).getOrElse(secondWord, Set.empty)
          documentsSoFar.intersect(matchingDocuments)
      }

      if (twoWordIndexResult.nonEmpty) {
        println("Two-word index matches the following documents:")
        twoWordIndexResult.toSeq.sorted.foreach(goodDocument => println(s" - $goodDocument"))
      } else {
        println("Two-word index does not match any documents")
      }

      println()
    }

    if(queryWords.nonEmpty) {

      println("Select max word distance for coordinate index:")

      val maxDistance = StdIn.readInt()

      val coordinateRevertIndexResult: Map[String, Seq[Int]] =
        queryWords.map(coordinateReverseIndex.positionsByWord(_).positionsByDocument).reduce[Map[String, Seq[Int]]]({
          case (positions1, positions2) =>
            positions1.map({
              case (document, wordPositions1) =>
                document -> positions2.get(document).map(wordPositions2 =>
                  CoordinateRevertIndex.coordinatedIntersection(wordPositions1, wordPositions2, maxDistance = maxDistance)
                ).getOrElse(Seq())
            }).filter(_._2.nonEmpty)
        })

      val bm = new BM25
      bm.setKeyFrequency(1)
      bm.setDocumentFrequency(coordinateRevertIndexResult.size)
      bm.setTermFrequency(coordinateRevertIndexResult.map(_._2.size).sum)
      bm.setNumberOfDocuments(allDocuments.size)
      bm.setAverageDocumentLength(avgDocumentLength)
      val pre = bm.precompute

      val docsByScores = SortedMap[Double, String](
        coordinateRevertIndexResult.map {
          case (doc, positions) =>
            (bm.score(positions.size, docLengths(doc)), doc)
        } toSeq : _*
      )

      System.out.println(bm.score(100, 200))
      System.out.println(bm.score(100, 200, pre))

      if (coordinateRevertIndexResult.nonEmpty) {
        println("Coordinate index matches the following documents:")
        docsByScores.toSeq.reverse.foreach {
          case (score, doc) => println(s"Document $doc; Score - $score")
        }
      } else println("Coordinate index does not match any documents")

      println()
    } else {
      println("Query is empty")
    }
  }
}