import scala.scalajs.js.annotation._

import vormenstoof._
import vormenstoof.Parser._

@JSExportTopLevel("Vormenstoof")
object Vormenstoof {
  @JSExport
  def parse(name: String, program: String): Int = {
    val trees = Parser.interpretIndentation(name, program).map(Parser.tokenize)
    val (context, segments) = Parser.parseSegments(trees, Context.empty)
    segments.length
  }
      
}
