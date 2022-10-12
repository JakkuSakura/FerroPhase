package rust
import com.typesafe.scalalogging.Logger
import shll.ast.Ast
import shll.backends.Backend

import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.nio.file.Files
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
case class RustCompiler(
    release: Boolean = false
) {
  def compileTo(source: String, path: File): Unit = {
    val commands = mutable.ArrayBuffer("rustc", "-", "--crate-name", path.getName)
    if (release)
      commands += "-O"
    val proc = ProcessBuilder(commands.asJava)
      .directory(path.getParentFile)
      .inheritIO()
      .redirectInput(Redirect.PIPE)
      .start()
    proc.getOutputStream.write(source.getBytes)
    proc.getOutputStream.close()
    var code = proc.waitFor()
    if (code != 0)
      throw Exception("Compilation failed, status code " + code)

    code = ProcessBuilder("chmod", "+x", path.toString).start().waitFor()
    if (code != 0)
      throw Exception("chmod failed, status code " + code)
  }
}
