package common

import java.io.PrintWriter

object MyFileWriter {
  def write(filename : String, content : String) : Unit = {
    new PrintWriter(filename) {
      write(content)
      close()
    }

    ()
  }
}
