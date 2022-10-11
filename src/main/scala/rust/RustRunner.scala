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
  override def process(node: Ast): Unit = {
    getRuntimeDuration(node)
  }

  def getRuntimeDuration(node: Ast): Long = {
    var code = RustPrettyPrinter().print(node)
    if (!code.contains("fn main()")) {
      code = augmentWithPrint(code)
    }
    code = Rustfmt().rustfmt(code)
    val path = Files.createTempFile("", "")
    logger.debug("Compiling to " + path + "\n" + code)
    RustCompiler().compileTo(code, path.toFile)
    var commands = List(path.toString)
    if (time)
      commands = List("bash", "-c", "time " + commands.mkString(" "))
    logger.debug("Running " + commands.mkString(" "))
    val begin = System.currentTimeMillis()
    val run = ProcessBuilder(commands: _*).inheritIO().start().waitFor()
    if (run != 0)
      throw Exception("Execution failed, status code " + run)
    System.currentTimeMillis() - begin
  }
}
