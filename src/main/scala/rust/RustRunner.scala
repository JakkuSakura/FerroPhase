package rust

import com.typesafe.scalalogging.Logger
import shll.ast.Ast
import shll.backends.Backend

import java.nio.file.Files

case class RustRunner(time: Boolean = true) extends Backend {
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

  def augmentWithMain(code: String): String = {
    s"""
       |fn main() {
       |  $code;
       |}
       |""".stripMargin

  }
  def compileExecutable(code0: String): String = {
    val code = Rustfmt().rustfmt(code0)
    val path = Files.createTempFile("", "")
    logger.debug("Compiling to " + path + "\n" + code)
    RustCompiler().compileTo(code, path.toFile)
    path.toAbsolutePath.toString
  }
  override def process(node: Ast): Unit = {
    var code = RustPrettyPrinter().print(node)
    if (!code.contains("fn main()")) {
      code = augmentWithPrint(code)
    }
    val path = compileExecutable(code)
    var commands = List(path)
    if (time)
      commands = List("bash", "-c", "time " + commands.mkString(" "))
    logger.debug("Running " + commands.mkString(" "))

    val run = ProcessBuilder(commands: _*).inheritIO().start().waitFor()
    if (run != 0)
      throw Exception("Execution failed, status code " + run)
  }

  def getRuntimeDuration(node: Ast): Long = {
    var code = RustPrettyPrinter().print(node)
    if (!code.contains("fn main()")) {
      code = augmentWithMain(code)
    }
    val path = compileExecutable(code)
    val commands = List(path)
    logger.debug("Running " + commands.mkString(" "))
    val begin = System.currentTimeMillis()
    val run = ProcessBuilder(commands: _*).inheritIO().start().waitFor()
    if (run != 0)
      throw Exception("Execution failed, status code " + run)
    System.currentTimeMillis() - begin
  }
}
