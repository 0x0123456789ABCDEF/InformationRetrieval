package practice7

/**
  * Created by alosha on 3/19/17.
  */
object Aliases {
  // AnyVal ensures there will be no runtime overhead
  case class ZoneName(name : String) extends AnyVal
  case class DocumentName(name : String) extends AnyVal
  case class ZonePriority(priority : Double) extends AnyVal
  case class DocumentPriority(priority : Double) extends AnyVal {
    def + (documentPriority: DocumentPriority) = DocumentPriority(priority + documentPriority.priority)
  }
  case class Term(word : String) extends AnyVal
}
