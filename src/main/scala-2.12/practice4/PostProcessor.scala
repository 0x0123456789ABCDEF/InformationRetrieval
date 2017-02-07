package practice4

/**
  * Created by alosha on 2/7/17.
  */
object PostProcessor extends ((Seq[String], Seq[String]) => Seq[String]) {
  override def apply(results: Seq[String], queryTokens: Seq[String]): Seq[String] = {
    def _apply(r : Seq[(String, String)], q: List[String]): Seq[String] = (r, q) match {
      case (res, List()) => res.map(_._1)
      case (resSoFar, tokenToMatch::restOfTokens) =>
        val normalizedTokenToMatch = if(tokenToMatch.head == '$') '^' + tokenToMatch.tail else tokenToMatch
        val tokenMatcher = s""".*?$normalizedTokenToMatch(.*)""".r
        _apply(
          resSoFar.collect {
            case (original, tokenMatcher(rest)) => (original, rest)
          },
          restOfTokens
        )
    }
    _apply(results.map(x => x -> x), queryTokens.toList)
  }
}
