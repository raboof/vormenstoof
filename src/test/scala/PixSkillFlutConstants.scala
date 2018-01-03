package vormenstoof

object PixSkillFlutConstants {
  import Parser._
  import Interpreter._
  val pixskillflutBuiltins: Map[String, (Seq[Value], State) => Value] = Map(
    "put_pixel" -> { case (args, state) =>
      println("Putting pixel, args: " + args)
      Value(42, Integer)
    }
  )
}