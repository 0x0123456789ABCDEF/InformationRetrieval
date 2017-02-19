package practice1

import common.{BookCollection, IntegerFormatter, MyFileWriter, CoordinateRevertIndex}

/**
  * @author alosha
  */
object Practice1 extends App {

      val reverseIndex = CoordinateRevertIndex.fromCollection(BookCollection)

      val result =
        s"""
           | Total character count: ${IntegerFormatter(BookCollection.getPaths.map(BookCollection.getSource).map(_.iter.size.toLong).sum)}
           | Total words count: ${IntegerFormatter(reverseIndex.positionsByWord.map(_._2.positionsByDocument.size).sum)}
           | Unique words count: ${IntegerFormatter(reverseIndex.positionsByWord.size)}
         """.stripMargin

      MyFileWriter.write("output_1.txt", result)
}