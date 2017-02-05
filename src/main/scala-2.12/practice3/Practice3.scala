package practice3

import common.{BookCollection, CoordinateRevertIndex, TwoWordRevertIndex}

import scala.io.StdIn





object Practice3 extends App {
   val coordinateReverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)
   val twoWordReverseIndex = TwoWordRevertIndex.fromCollection(BookCollection)

   while(true) {
      print("Please type your multiword query: ")

      val query = StdIn.readLine

      val allDocuments = BookCollection.getPaths.toSet

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

        val coordinateRevertIndexResult: Set[String] =
          queryWords.map(coordinateReverseIndex.positionsByWord(_).positionsByDocument).reduce[Map[String, Seq[Int]]]({
            case (positions1, positions2) =>
              positions1.map({
                case (document, wordPositions1) =>
                  document -> positions2.get(document).map(wordPositions2 =>
                    CoordinateRevertIndex.coordinatedIntersection(wordPositions1, wordPositions2, maxDistance = maxDistance)
                  ).getOrElse(Seq())
              }).filter(_._2.nonEmpty)
          }).keySet

        if (coordinateRevertIndexResult.nonEmpty) {
          println("Coordinate index matches the following documents:")
          coordinateRevertIndexResult.toSeq.sorted.foreach(goodDocument => println(s" - $goodDocument"))
        } else {
          println("Coordinate index does not match any documents")
        }

        println()
      } else {
        println("Query is empty")
      }
   }
}
