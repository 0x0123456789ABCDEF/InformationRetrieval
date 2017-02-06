package practice4

import scala.collection.SortedMap

/**
  * Created by alosha on 2/6/17.
  */
class PermutationIndex(terms : Seq[String]) {
  val data : SortedMap[String, String] = SortedMap(
    terms.flatMap(term => (term.inits zip term.tails.toSeq.reverseIterator).map {
      case (init, tail) => (tail + "$" + init) -> term
    }) : _*
  )

  private val beginEndMatch = """^([^*]*)\*([^*]*)$""".r
  def queryWithOneWildcard(q : String) : Iterable[String] = q match {
    case "" => data.values
    case beginEndMatch(begin, end) =>
      val pq = end + "$" + begin
      data.range(pq, pq.substring(0, pq.length - 1) + (pq.charAt(pq.length - 1) + 1).toChar).values
  }
}
