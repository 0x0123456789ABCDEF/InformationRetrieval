package practice4

import scala.collection.SortedMap

/**
  * Created by alosha on 2/7/17.
  */
class _3GramIndex(terms : Seq[String]) {
  val data : SortedMap[String, Set[String]] =
    SortedMap(terms.flatMap(term => ("$" + term.toLowerCase + "$").sliding(3).map(_ -> term)).groupBy(_._1).mapValues(_.map(_._2).toSet).toSeq : _*)

  def queryWithWildcards(query : String) : Seq[String] = {
    var queryTokens = ("$" + query.trim.toLowerCase + "$").split("\\*")
    PostProcessor(kGramQuery(queryTokens).toSeq, queryTokens)
  }

  private def kGramQuery(queryTokens : Seq[String]) : Set[String] =
    queryTokens.flatMap(token => if(token.length > 3) token.sliding(3) else Iterator.single(token)).collect({
      case s if s.length == 3 => data.getOrElse(s, Set.empty)
      case s if s.length < 3 && !s.last.equals('$') => data.range(s, s.init + (s.last + 1).toChar).values.fold(Set.empty)(_ ++ _)
    }).reduceOption(_ intersect _).getOrElse(data.flatMap(_._2).toSet)
}
