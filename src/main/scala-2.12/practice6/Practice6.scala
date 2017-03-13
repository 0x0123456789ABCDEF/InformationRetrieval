package practice6

import common.BigCollection
import practice5.BlockedSortBasedIndexing

/**
  * Created by alosha on 3/13/17.
  */
object Practice6 extends App {
  BlockedSortBasedIndexing.processCollection(BigCollection)
  val zippedIndex = ZippedIndexFromFile(BigCollection.getPaths.head)
}
