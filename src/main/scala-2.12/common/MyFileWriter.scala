package common

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object MyFileWriter {
  def write(filename : String, content : String) : Unit = {
    Files.write(Paths.get(filename), content.getBytes(StandardCharsets.UTF_8))
  }
}
