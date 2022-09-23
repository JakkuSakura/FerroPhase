package rust
import com.typesafe.scalalogging.Logger
import shll.ast.AST
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
case class RustRunnerBackend(time: Boolean = true) extends Backend {
  val logger: Logger = Logger[this.type]
  def augmentWithPrint(code: String): String = {
    s"""
      |fn main() {
      |  println!("{:?}", {
      |    $code
      |  });
      |}
      |""".stripMargin

  }
  override def process(node: AST): Unit = {
    var code = RustPrettyPrinter().print(node)
    if (!code.contains("fn main()")) {
      code = augmentWithPrint(code)
    }

    val path = Files.createTempFile("", "")
    logger.debug("Compiling to " + path + "\n" + code)
    RustCompiler().compileTo(code, path.toFile)
    var commands = List(path.toString)
    if (time)
      commands = List("bash", "-c", "time " + commands.mkString(" "))
    logger.debug("Running " + commands.mkString(" "))
    val run = ProcessBuilder(commands: _*).inheritIO().start().waitFor()
    if (run != 0)
      throw Exception("Execution failed, status code " + run)
  }
}
