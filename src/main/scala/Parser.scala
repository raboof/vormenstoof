package vormenstoof

object Parser {
  case class Location(file: String, line: Int, at: Int) {
    override val toString = s"$file:$line,$at"
  }
  object Location {
    val unknown = Location("", -1, -1)
    def unknown(filename: String) = Location(filename, -1, -1)
  }
  class ParseError(message: String, val at: Location) extends Throwable(s"$at: $message")

  case class Tree(top: String, children: Seq[Tree] = Seq.empty, location: Location) {
    def single = {
      require(children.isEmpty)
      top
    }
    def drop(n: Int): Tree = n match {
      case 0 => this
      case n => Tree(children.head.top, children.tail, children.head.location).drop(n - 1)
    }
  }

  private def depth(line: String): Int = line.takeWhile(_ == ' ').size

  // Put the lines of the program in a tree structure by just looking at indentation
  def interpretIndentation(filename: String, text: String): Seq[Tree] = {
    case class ParseResult(trees: Seq[Tree], remaining: Seq[(String, Int)])
    object ParseResult {
      val empty = new ParseResult(Seq.empty, Seq.empty)
    }

    def parseLines(lines: Seq[(String, Int)], atDepth: Int): ParseResult = {
      if (lines.isEmpty) ParseResult.empty
      else {
        val (line, lineno) = lines.head
        val location = Location(filename, lineno, depth(line))
        if (depth(line) == atDepth) {
          val ParseResult(children, rest) = parseLines(lines.tail, atDepth + 2)
          val ParseResult(peers, rest2) = parseLines(rest, atDepth)
          ParseResult(Tree(line.drop(atDepth), children, location) +: peers, rest2)
        } else if (depth(line) < atDepth) {
          ParseResult(Seq.empty, lines)
        } else {
          throw new ParseError("Invalid indentation", location)
        }
      }
    }

    val result = parseLines(
      text
        .split("\n")
        .zipWithIndex
        .filter { case (line, _) => !line.isEmpty }
        .map { case (line, idx) => (line, idx + 1) }
        .toSeq, atDepth = 0)

    require(result.remaining.isEmpty)

    result.trees
  }

  case class Context(val types: Map[String, Type], val variables: Map[String, Type])
  object Context {
    def apply(t: Type) = new Context(Map(t.name -> t), Map.empty)
    val empty = new Context(Map.empty, Map.empty)
  }

  def parseAliasOpt(tree: Tree, context: Context): Option[Alias] = {
    tree.children.reverse match {
      case Seq(name, named, t@_*) if named.top == "named" =>
        Some(Alias(parseType(Tree(tree.top, t, tree.location), context), name.top))
      case _ =>
        None
    }
  }

  def parsePrimitiveOpt(tree: Tree): Option[Primitive] = {
    if (tree.top == "int") Some(Integer)
    else None
  }

  def parseNonPrimitiveOpt(tree: Tree, context: Context): Option[Type] =
    context.types.get(tree.single)

  def parseType(tree: Tree, context: Context): Type = {
    parseAliasOpt(tree, context)
      .orElse(parseNonPrimitiveOpt(tree, context))
      .orElse(parsePrimitiveOpt(tree))
      .getOrElse(throw new ParseError(s"Could not parse type $tree", tree.location))
  }

  def parseRecordOpt(tree: Tree, context: Context): Option[Record] = {
    if (tree.top != "record") None
    else Some(Record(tree.children.head.single, tree.children.tail.map(child => parseType(child, context))))
  }

  def parseMethodCall(tree: Tree, context: Context): MethodCall =
    // TODO do we add all method declarations to the context or postpone
    // checking for methods and determining the result type to a later phase?
    // might be good to postpone to allow circular references, recursion etc
    MethodCall(tree.top, tree.children.map(child => parseExpression(child, context)), null)

  def parseReferenceOpt(tree: Tree, context: Context): Option[Reference] =
    context.variables.get(tree.top).map(t => Reference(tree.top, t))

  def parseLiteralOpt(tree: Tree): Option[Literal] = {
    if (tree.top.head.isDigit) Some(Literal(tree.top.toInt, Integer))
    else None
  }

  def parseExpression(tree: Tree, context: Context): Expression = {
    parseLiteralOpt(tree)
      .orElse(parseReferenceOpt(tree, context))
      .getOrElse(parseMethodCall(tree, context))
  }

  def parseAssignmentOpt(tree: Tree, context: Context): Option[(Assignment, Context)] = {
    val assignment =
      if (tree.children.size > 2 && tree.children(2).top == "=")
        Some(Assignment(tree.children(1).single, parseExpression(tree.drop(4), context)))
      else if (tree.children.size >= 1 && tree.children(0).top == "=")
        Some(Assignment(tree.top, ???))
      else
       None

     assignment.map(a => (a, context.copy(variables = context.variables.updated(a.name, a.value.t))))
  }

  def parseSegment(tree: Tree, context: Context): (Segment, Context) = {
    parseRecordOpt(tree, context).map(r => (r, context))
      .orElse(parseAssignmentOpt(tree, context))
      .getOrElse((parseMethodCall(tree, context), context))
  }

  // Recursive with the depth of the tree - should be fine.
  def tokenize(string: String, location: Location): Seq[Tree] = string.headOption match {
    case None =>
      Seq.empty
    case Some(' ') =>
      tokenize(string.tail, location.copy(at = location.at + 1))
    // TODO string literals
    // TODO parenthesized subtrees
    case Some(other) =>
      val token = string.takeWhile(_ != ' ')
      Tree(token, Seq.empty, location) +: tokenize(string.drop(token.size), location.copy(at = location.at + token.size))
  }

  def tokenize(tree: Tree): Tree = {
    val tokens = tokenize(tree.top, tree.location)
    Tree(tokens.head.single, tokens.tail ++ tree.children.map(tokenize), tree.location)
  }

  // Recursive to the number of segments - should be fine.
  def parseSegments(trees: Seq[Tree], context: Context): (Context, Seq[Statement]) = trees match {
    case Seq() => (context, Seq.empty)
    case Seq(head, tail@_*) => parseSegment(head, context) match {
      case (t: Type, context: Context) =>
        if (context.types.keys.toSet.contains(t.name)) throw new ParseError(s"Duplicate definition of ${t.name}", head.location)
        else parseSegments(tail, context.copy(types = context.types.updated(t.name, t)))
      case (stm: Statement, context: Context) =>
        println(s"Parsed $stm, ctx now $context")
        val (c, s) = parseSegments(tail, context)
        (c, stm +: s)
    }
  }

  def apply(filename: String, text: String, after: Program = Program.empty): Program = {
    try {
      val trees = interpretIndentation(filename, text)
      val tokenized = trees.map(tokenize)
      val (context, statements) = parseSegments(tokenized, after.context)

      Program(after.statements ++ statements, context)
    } catch {
      case pe: ParseError =>
        println(s"Parse error at ${pe.at}")
        throw pe
    }
  }

  def fromResource(resourceName: String, after: Program = Program.empty): Program = {
     val text = io.Source.fromInputStream(getClass.getResourceAsStream(resourceName)).mkString
     Parser(resourceName, text, after)
  }
}