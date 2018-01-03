package vormenstoof

object Interpreter {
  case class State(currentStatement: Int, variables: Map[String, Value])
  object State {
    val empty = State(0, Map.empty)
  }
}

import Interpreter._
class Interpreter(program: Program, builtins: Map[String, (Seq[Value], State) => Value]) {
  var state: State = State.empty

  def evaluate(expression: Expression): Value = expression match {
    case v: Value =>
      v
    case r: Reference =>
      println(state.variables)
      state.variables(r.name)
    case MethodCall(method, arguments, _) =>
      // for now side effects are side effects, will probably change
      builtins(method)(arguments.map(evaluate), state)
  }

  def execute(statement: Statement): Unit = statement match {
    case Assignment(to, expression) =>
      state = state.copy(
        variables = state.variables.updated(to, evaluate(expression)),
        currentStatement = state.currentStatement + 1)
      println(s"After assignment variables are ${state.variables}")
    case m: MethodCall =>
      evaluate(m)
      state = state.copy(currentStatement = state.currentStatement + 1)
  }
  def step(): Unit = execute(program.statements(state.currentStatement))

  def run(): State =
    if (state.currentStatement == program.statements.size)
      state
    else {
      step()
      run()
    }
}