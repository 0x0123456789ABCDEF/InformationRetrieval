package practice8

object Aliases {
  // AnyVal ensures there will be no runtime overhead
  case class ZoneName(name : String) extends AnyVal
  case class DocumentName(name : String) extends AnyVal
  case class DocumentStaticValue(value : Double) extends AnyVal
  case class Term(word : String) extends AnyVal
}
