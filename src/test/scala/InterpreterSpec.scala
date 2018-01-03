package vormenstoof

import org.scalatest._

class InterpreterSpec extends WordSpec with Matchers {
  "The interpreter" should {
    import Interpreter._

    import PixSkillFlutConstants._

    "run a simple pixskillflut program" in {
      val preamble = Parser.fromResource("/pixskillflut-preamble.vs")
      val program = Parser.fromResource("/pixels.vs", preamble)
      val i = new Interpreter(program, pixskillflutBuiltins)
      val effects = i.run()
    }
  }
}