package shll

import com.typesafe.scalalogging.Logger
import org.junit.jupiter.api.Test
import shll.backends.{PrettyPrinter, ShllPrettyPrinter}

class TestKind {
  val logger: Logger = Logger[this.type]
  val pp: PrettyPrinter = ShllPrettyPrinter()

}
