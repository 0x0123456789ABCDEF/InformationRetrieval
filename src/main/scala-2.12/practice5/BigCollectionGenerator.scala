package practice5

import java.io.{BufferedWriter, FileWriter}

import scala.util.Random



object BigCollectionGenerator extends App {
  implicit class SeqRandomOps[T](seq : Seq[T]) {
    def randomElem = {
      seq(Random.nextInt(seq.size))
    }
  }

  val wordSymbols = (('a' to 'z') ++ ('A' to 'Z') ++ ('1' to '9')).toVector

  def genRandomWord(sizeRange: Range): String = {
    val length = sizeRange.randomElem
    (1 to length).map(_ => wordSymbols.randomElem).mkString
  }


  def generate(filesCnt: Int, wordsCntPerLine: Range, linesCntPerDoc: Range, wordsSizeRng: Range) = {

    val cachedRandomWords = (1 to 50000).map(_ => genRandomWord(wordsSizeRng)).toVector

    println("Random words were cached")
    //println(cachedRandomWords.randomElem)

    def generateDocument(filename: String) = {
      val writer = new BufferedWriter(new FileWriter(filename))
      val linesCnt = linesCntPerDoc.randomElem
      println(s"lines cnt - $linesCnt")
      (1 to linesCnt).foreach(lineI => {
        if(lineI % 100000 == 0)
        println(lineI)
        val wordsCnt = wordsCntPerLine.randomElem
        (1 to wordsCnt).foreach(_ => {
//          println("kek")
//          println(cachedRandomWords.randomElem)
          writer.write(cachedRandomWords.randomElem)
          writer.write(' ')
        })
        writer.write('\n')
      })
      writer.flush()
      writer.close()
    }

    (1 to filesCnt).foreach(i => {
      generateDocument(s"bigdoc$i.txt")
    })
  }

  generate(10, 1 to 20, 10000000 to 20000000, 1 to 20)
}
