package practice7

import Aliases._

/**
  * Created by alosha on 3/19/17.
  */
trait ZonedCollection {
 def documents : Seq[ZonedDocument]
 def zonePriorities : Map[ZoneName, ZonePriority]
 def zones : Seq[ZoneName] = zonePriorities.keySet.toSeq
}
