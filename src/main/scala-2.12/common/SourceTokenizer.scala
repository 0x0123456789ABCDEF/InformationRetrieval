package common

import scala.io.{BufferedSource, Source}

object SourceTokenizer extends (Source => Seq[String]) {
  override def apply(documentSource: Source): Seq[String] =
      documentSource.getLines().flatMap(line => {
        val tokens = line.split("[^\\w]+")
        val nonEmptyTokens = tokens.filter(_.nonEmpty)
        val normalizedTokens = nonEmptyTokens.map(TokenNormalizer)
        nonEmptyTokens
      }).toSeq
}
