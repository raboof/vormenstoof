package vormenstoof

object Interpreter {
  case class State(currentStatement: Int, variables: Map[String, Value])
  trait Effect {
    def and(other: Effect): Effect = other match {
      case Nothing => this
      case _ => Effects(this, other)
    }
  }
  case class Effects(one: Effect, other: Effect) extends Effect
  case object Nothing extends Effect {
    override def and(other: Effect) = other
  }
  object State {
    val empty = State(0, Map.empty)
  }
}

import Interpreter._
class Interpreter(program: Program, builtins: Map[String, (Seq[Value], State) => (Value, Effect)]) {
  val methods: Map[String, (Seq[Value], State) => (Value, Effect)] =
    builtins ++ deriveConstructors(program.types.values)
  var state: State = State.empty

  // Probably needs to move out of the interpreter to a separate typechecking/'linking' phase
  // but fine here for now
  def deriveConstructors(types: Iterable[Type]): Map[String, (Seq[Value], State) => (Value, Effect)] =
    types.collect { case r: Record => r.name -> {
      (arguments: Seq[Value], s: State) => (Value(arguments, r), Nothing)
    }}.toMap

  def evaluate(expression: Expression): (Value, Effect) = expression match {
    case l: Literal =>
      (Value(l.value, l.t), Nothing)
    case r: Reference =>
      val value = state.variables(r.name)
      println(s"Evaluating ${r.name} to ${Printer.print(value)} in ${state.variables.map{ case (k, v) => k + " = " + Printer.print(v)}}")
      (value, Nothing)
    case MethodCall(method, arguments, _) =>
      val evaluated = arguments.map(evaluate)
      val effects = evaluated.map(_._2).reduce((l: Effect, r: Effect) => l.and(r))
      val (value, effect) = methods(method)(evaluated.map(_._1), state)
      (value, effects.and(effect))
  }

  def execute(statement: Statement): Effect = statement match {
    case Assignment(to, expression) =>
      val (value, effect) = evaluate(expression)
      state = state.copy(
        variables = state.variables.updated(to, value),
        currentStatement = state.currentStatement + 1)
      effect
    case m: MethodCall =>
      val (_, effect) = evaluate(m)
      state = state.copy(currentStatement = state.currentStatement + 1)
      effect
  }
  def step(): Effect = execute(program.statements(state.currentStatement))

  def run(): Effect =
    if (state.currentStatement == program.statements.size)
      Nothing
    else {
      step().and(run())
    }
}