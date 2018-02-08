package vormenstoof

object Printer {

  def print(value: Value): String = value.t match {
    case Integer => value.value.toString
    case Record(name, fields) =>
      name + " { " + fields.zip(value.value.asInstanceOf[List[Value]]).map { case (t, v) => t.name + " = " + print(v) }.mkString("(", ") (", ")") + "}"
    case Alias(t, name) => "alias " + t + " " + name
  }
}