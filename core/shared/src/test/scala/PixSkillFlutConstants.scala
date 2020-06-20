package vormenstoof

object PixSkillFlutConstants {
  import Parser._
  import Interpreter._

  case class PutPixel(x: Int, y: Int) extends Effect

  val pixskillflutBuiltins: Map[String, (Seq[Value], State) => (Value, Effect)] = Map(
    "put_pixel" -> { case (args, state) =>
      println("Putting pixel, args: " + args.map(Printer.print).mkString(", "))
      (Value(42, Integer), PutPixel(args(1).value.asInstanceOf[Int], args(2).value.asInstanceOf[Int]))
    }
  )
}