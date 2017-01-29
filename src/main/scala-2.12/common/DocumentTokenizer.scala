package common

import scala.io.Source

object DocumentTokenizer extends (String => Seq[String]) {
  override def apply(documentPath: String): Seq[String] =
      Source.fromResource(documentPath).getLines().flatMap(line => {
        val tokens = line.split("[^\\w]+")
        val nonEmptyTokens = tokens.filter(_.nonEmpty)
        val normalizedTokens = nonEmptyTokens.map(TokenNormalizer)
        nonEmptyTokens
      }).toSeq
}
