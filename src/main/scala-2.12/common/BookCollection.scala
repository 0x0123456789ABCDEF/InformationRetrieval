package common

object BookCollection extends DocumentCollection {
  def getPaths : Seq[String] =
    (1 to 10).map("book" + _ + ".txt")
}
