package practice11

import practice7.Aliases._
import practice7._

/**
  * Created by alosha on 4/9/17.
  */
object FB2ZonedCollection {
  def fromFiles(files : String*) : ZonedCollection =
    new ZonedCollection {
      override def documents: Seq[ZonedDocument] = files.map(FB2ZonedDocument.fromFile)

      override def zonePriorities: Map[ZoneName, ZonePriority] = Map(
        ZoneName("author") -> ZonePriority(1),
        ZoneName("keywords") -> ZonePriority(0.8),
        ZoneName("date") -> ZonePriority(0.4),
        ZoneName("title") -> ZonePriority(1),
        ZoneName("body") -> ZonePriority(0.6)
      )
    }

}
