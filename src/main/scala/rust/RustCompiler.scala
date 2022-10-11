package rust
import com.typesafe.scalalogging.Logger
import shll.ast.Ast
import shll.backends.Backend

import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.nio.file.Files

case class RustCompiler() {
  def compileTo(source: String, path: File): Unit = {
    val proc = ProcessBuilder("rustc", "-", "--crate-name", path.getName)
      .directory(path.getParentFile)
      .redirectOutput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
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

