package practice4

import scala.collection.SortedSet

/**
  * Created by alosha on 2/6/17.
  */
class TermTree(terms : Seq[String]) {
  val data = SortedSet[String](terms : _*)
  val reverseData = SortedSet[String](terms.map(_.reverse) : _*)

  def queryByBeginning(q : String) : Set[String] = q match {
    case "" => data.toSet
    case _ => data.range(q, q.substring(0, q.length - 1) + (q.charAt(q.length - 1) + 1).toChar).toSet
  }

  def queryByEnd(q : String) : Set[String] = q match {
    case "" => data.toSet
    case _ =>
      val rq = q.reverse
      reverseData.range(rq, rq.substring(0, rq.length - 1) + (rq.charAt(rq.length - 1) + 1).toChar).map(_.reverse).toSet
  }

  private val beginEndMatch = """^([^*]*)\*([^*]*)$""".r
  def queryWithOneWildcard(q : String) : Set[String] = q match {
    case "" => data.toSet
    case beginEndMatch(begin, end) => queryByBeginning(begin).intersect(queryByEnd(end))
  }


}
