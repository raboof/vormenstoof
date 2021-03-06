package vormenstoof

sealed trait Segment

sealed trait Type extends Segment {
  val name: String
}

/**
 * @param auto true if types can be converted automatically ('is any'), false if they cannot ('is a(n)')
 */
case class Alias(from: Type, name: String, auto: Boolean) extends Type

sealed trait Primitive extends Type
case object Integer extends Primitive {
  val name = "int"
}
case class Record(name: String, types: Seq[Type]) extends Type
case class TypeNotFound(name: String) extends Type


/** Something that resolves to a value */
sealed trait Expression {
  val t: Type
}
case class Literal(value: Any, t: Type) extends Expression
case class Reference(name: String, t: Type) extends Expression

sealed trait Statement extends Segment
case class MethodCall(method: String, arguments: Seq[Expression], t: Type) extends Statement with Expression
// TODO Not sure we should have both 'name' and 't'
case class Assignment(name: String, value: Expression, t: Type) extends Statement with Expression

case class Program(statements: Seq[Statement], context: Parser.Context) {
  def +(filename: String, text: String) = Parser(filename, text, this)
  def types: Map[String, Type] = context.types
}
object Program {
  val empty = Program(Seq.empty, Parser.Context.empty)
}

case class Value(value: Any, t: Type)