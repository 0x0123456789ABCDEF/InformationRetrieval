package practice8

import practice8.Aliases.{DocumentName, DocumentStaticValue}

import scala.io.BufferedSource
import Aliases._
import common.SourceTokenizer

/**
  * Created by alosha on 3/26/17.
  */
trait StaticRankedDocument extends Ordered[StaticRankedDocument] {
    val name : DocumentName
    val staticRank : DocumentStaticValue
    val source : BufferedSource

    lazy val termSet : Set[Term] = Set(SourceTokenizer(source).map(Term) : _*)

    lazy val vector : Vector[Int] = {
      val termIds : Set[Int] = termSet.map(term => TermCollection.getId(term.word))
      val maxTermId = termIds.max
      (0 to maxTermId).map {
        i => if(termIds.contains(i)) 1 else 0
      }.toVector
    }

    def cosineSimilarity(d2: StaticRankedDocument) : Double = {
      val numenator : Int = (vector zip d2.vector map ((_: Int) * (_: Int)).tupled).sum
      val denominator : Double = Seq(vector, d2.vector).map(_.map(x => x*x).sum).map(x => Math.sqrt(x)).product
      numenator / denominator
    }

    def cosineSimilarity(d2 : Set[Term]) : Double =
      cosineSimilarity(new StaticRankedDocument {
        override val source: BufferedSource = null
        override val staticRank: DocumentStaticValue = DocumentStaticValue(0)
        override val name: DocumentName = DocumentName("")
        override lazy val termSet = d2
      })

    // Sort descending by static rank
    override def compare(that: StaticRankedDocument) = that.staticRank.value compare staticRank.value
}
