package practice4

import common.{BookCollection, CoordinateRevertIndex, TwoWordRevertIndex}

import scala.io.StdIn


object Practice4 extends App {
   val coordinateReverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)
   val twoWordReverseIndex = TwoWordRevertIndex.fromCollection(BookCollection)

   val allTerms = coordinateReverseIndex.positionsByWord.keySet
   val termTree = new TermTree(allTerms.toSeq)
   val permIndex = new PermutationIndex(allTerms.toSeq)

   while(true) {
      print("Please type your multiword query: ")

      val query = StdIn.readLine

      val allDocuments = BookCollection.getPaths.toSet


      println(termTree.queryWithOneWildcard(query))
      println(permIndex.queryWithOneWildcard(query))

      val queryWords = query.split("\\s+")


   }
}
