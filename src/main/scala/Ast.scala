package vormenstoof

sealed trait Segment

sealed trait Type extends Segment {
  val name: String
}
case class Alias(from: Type, name: String) extends Type
sealed trait Primitive extends Type
case object Integer extends Primitive {
  val name = "int"
}
case class Record(name: String, types: Seq[Type]) extends Type

/** Something that resolves to a value */
sealed trait Expression {
  val t: Type
}
case class Literal(value: Any, t: Type) extends Expression
case class Reference(name: String, t: Type) extends Expression

sealed trait Statement extends Segment
case class MethodCall(method: String, arguments: Seq[Expression], t: Type) extends Statement with Expression
case class Assignment(name: String, value: Expression) extends Statement

case class Program(statements: Seq[Statement], context: Parser.Context) {
  def +(filename: String, text: String) = Parser(filename, text, this)
  def types: Map[String, Type] = context.types
}
object Program {
  val empty = Program(Seq.empty, Parser.Context.empty)
}

case class Value(value: Any, t: Type)