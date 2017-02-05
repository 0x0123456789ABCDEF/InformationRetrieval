package practice2

import common.{BookCollection, CoordinateRevertIndex}

import scala.io.StdIn





object Practice2 extends App {
   val reverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)

   while(true) {
      print("Please type your query (you can use AND/OR): ")

      val query = StdIn.readLine

      val allDocuments = BookCollection.getPaths

      // "OR" has lower precedence than "AND"
      val orGroups = query.split("(\\s|^)[oO][rR](\\s|$)")

      val resultsForGroups : Seq[Seq[String]] =
         orGroups.map(orGroup => {
           val andGroups = orGroup.split("(\\s|^)[aA][nN][dD](\\s|$)")
           andGroups.foldLeft(allDocuments) {
              case (Seq(), _) => Seq()
              case (documentsSoFar, andWord) =>

               val goodDocuments : Set[String] =
                 reverseIndex.positionsByWord
                   .get(andWord.trim.toLowerCase)
                   .map(_.positionsByDocument.keySet)
                   .getOrElse(Set.empty)

               if(goodDocuments.nonEmpty)
                  documentsSoFar.filter(goodDocuments.contains)
               else
                  Seq()
           }
         })

      val result : Set[String] =
        resultsForGroups.foldLeft(Set.empty[String]) {
          case (accum, orGroupResult) =>
            accum ++ orGroupResult
        }

      if(result.nonEmpty) {
        println("Query matches the following documents:")
        result.toSeq.sorted.foreach(goodDocument => println(s" - $goodDocument"))
      } else {
        println("Query does not match any elements")
      }

      println()
   }
}
