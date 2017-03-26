package practice8

import practice8.Aliases._

import scala.io.{BufferedSource, Source, StdIn}

/**
  * Created by alosha on 3/19/17.
  */
object Practice8 extends App {
  val testCollection = new StaticRankedCollection {
    override val documents = Seq(
      new StaticRankedDocument {
        override val source: BufferedSource = Source.fromFile("doc1.txt")
        override val staticRank = DocumentStaticValue(0.5)
        override val name = DocumentName("doc1")
      },
      new StaticRankedDocument {
        override val source: BufferedSource = Source.fromFile("doc2.txt")
        override val staticRank = DocumentStaticValue(0.7)
        override val name = DocumentName("doc2")
      },
      new StaticRankedDocument {
        override val source: BufferedSource = Source.fromFile("doc3.txt")
        override val staticRank = DocumentStaticValue(0.3)
        override val name = DocumentName("doc3")
      },
      new StaticRankedDocument {
        override val source: BufferedSource = Source.fromFile("doc4.txt")
        override val staticRank = DocumentStaticValue(0.9)
        override val name = DocumentName("doc4")
      },
      new StaticRankedDocument {
        override val source: BufferedSource = Source.fromFile("doc5.txt")
        override val staticRank = DocumentStaticValue(0.4)
        override val name = DocumentName("doc5")
      }
    )
  }

  val index = StaticRankedReverseIndex.fromCollection(testCollection)

  while(true) {
    val query : String = StdIn.readLine()
    val terms : Seq[Term] = query.split("[\\w+]").map(_.toLowerCase).map(Term)
    val result : Seq[StaticRankedDocument] = index.getSortedResultsByTerms(terms.toSet)
    result.foreach { doc =>
        println(doc.name)
    }
  }
}
