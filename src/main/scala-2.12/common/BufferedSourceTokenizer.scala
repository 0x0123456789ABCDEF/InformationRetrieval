package common

import scala.io.{BufferedSource, Source}

object BufferedSourceTokenizer extends (BufferedSource => Seq[String]) {
  override def apply(documentSource: BufferedSource): Seq[String] =
      documentSource.getLines().flatMap(line => {
        val tokens = line.split("[^\\w]+")
        val nonEmptyTokens = tokens.filter(_.nonEmpty)
        val normalizedTokens = nonEmptyTokens.map(TokenNormalizer)
        nonEmptyTokens
      }).toSeq
}
