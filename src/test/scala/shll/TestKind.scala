package shll

class TestKind {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()

  @Test def testKind(): Unit = {
    TestHelper.parseCode(
      """
        |""".stripMargin)
  }

}
