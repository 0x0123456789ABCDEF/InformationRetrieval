package common

object BookCollection extends Collection {
  def getPaths : Seq[String] =
    (1 to 10).map("book" + _ + ".txt")
}
