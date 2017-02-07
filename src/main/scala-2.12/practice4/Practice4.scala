package practice4

import common.{BookCollection, CoordinateRevertIndex, TwoWordRevertIndex}

import scala.io.StdIn
import scalaz._, Scalaz._


object Practice4 extends App {
   val coordinateReverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)
   val twoWordReverseIndex = TwoWordRevertIndex.fromCollection(BookCollection)

   val allTerms = coordinateReverseIndex.positionsByWord.keySet
   val termTree = new TermTree(allTerms.toSeq)
   val permIndex = new PermutationIndex(allTerms.toSeq)
   val kGramIndex = new _3GramIndex(allTerms.toSeq)

   while(true) {
      print("Please type your multiword query: ")

      val query = StdIn.readLine

      val allDocuments = BookCollection.getPaths.toSet


//      println(termTree.queryWithOneWildcard(query))
//      println(permIndex.queryWithOneWildcard(query))


      val queryWords = query.split("\\s+")


      val maxDistance =
        if(queryWords.length > 1) {
          print("Select max word distance for coordinate index:")
          StdIn.readInt()
        } else {
          0
        }

      val result = queryWords.par.map(kGramIndex.queryWithWildcards).map(
        _.map(coordinateReverseIndex.positionsByWord(_).positionsByDocument.mapValues(_.toVector)).reduceOption(_ |+| _).getOrElse(Map.empty).mapValues(_.toSeq)
      ).reduceOption[Map[String,Seq[Int]]]({
        case (positionsSoFar, positionsToIntersect) =>
          positionsSoFar.keysIterator.collect({
            case document if positionsToIntersect.contains(document) =>
              document -> CoordinateRevertIndex.coordinatedIntersection(positionsSoFar(document), positionsToIntersect(document), maxDistance)
          }).toMap
      }).getOrElse(Map.empty)


      result.foreach({
        case (document, positions) => positions.foreach(pos => println(s"$document; pos $pos"))
      })

      println(s"Found ${result.map(_._2.size).sum} matches:")

      println()

   }
}
