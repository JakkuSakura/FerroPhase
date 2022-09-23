package rust

import java.lang.ProcessBuilder.Redirect
import scala.io.Source

case class Rustfmt () {
  def rustfmt(source: String): String = {
    val proc = ProcessBuilder("rustfmt")
      .redirectError(Redirect.INHERIT)
      .start()
    proc.getOutputStream.write(source.getBytes)
    proc.getOutputStream.close()
    val code = proc.waitFor()
    if (code != 0)
      throw Exception("Rustfmt failed, status code " + code)
    Source.fromInputStream(proc.getInputStream).mkString("")
  }
}
