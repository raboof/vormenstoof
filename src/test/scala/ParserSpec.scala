package vormenstoof

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "The parser" should {
    "parse 3 levels of indentation into the correct tree" in {
      import Parser._

      Parser.interpretIndentation("test.txt", """
        |foo
        |  bar
        |    baz""".stripMargin) match {
  case List(Tree("foo", Seq(Tree("bar", Seq(Tree("baz", _, _)), _)), _)) => // ok
}
    }

    "parse returning to the previous level of indentation" in {
      import Parser._

      Parser.interpretIndentation("test.txt", """
|foo
|  bar
|    baz
|  qux""".stripMargin) match {
  case List(
    Tree("foo", Seq(
      Tree("bar", Seq(Tree("baz", _, _)), _),
      Tree("qux", _, _)), _
    )) => // ok
}
    }

    "warn about invalid indentation" in {
      import Parser._

      try {
        Parser.interpretIndentation("test.txt", """
          |foo
          |  bar
          |     baz
          |  qux""".stripMargin)
      } catch {
        case e: ParseError =>
          e.getMessage should be("test.txt:4,5: Invalid indentation")
          e.at should be(Location("test.txt", line = 4, at = 5))
      }
    }

    "tokenize 'is an' as a separate token" in {
      import Parser._

      val tree = Parser.tokenize(Tree("r is an int", Seq.empty, Location.unknown))
      tree.top should be ("r")
      tree.children.size should be(2)
      tree.children(0).top should be("is an")
      tree.children(1).top should be("int")
    }

    "parse a named variable of primitive type" in {
      import Parser._

      val tree = Parser.tokenize(Tree("r is an int", Seq.empty, Location.unknown))
      Parser.parseAliasOpt(tree, Context.empty) should be(Some(Alias(Integer, "r", auto = false)))
      Parser.parseType(tree, Context.empty) should be(Alias(Integer, "r", auto = false))
    }

    "parse a named variable of composite type" in {
      import Parser._

      val compositeType = Record("color", Seq.empty)
      val tree = Parser.tokenize(Tree("r is a color", Seq.empty, Location.unknown))
      Parser.parseAliasOpt(tree, Context(compositeType)) should be(Some(Alias(compositeType, "r", auto = false)))
    }

    "parse an assignment of a literal to a primitive variable" in {
      import Parser._

      val trees = Parser.interpretIndentation("test.vs", "int = 42").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("int", Literal(42, Integer)))
    }

    "parse an assignment of a literal to a variable with an explicit type" in {
      import Parser._

      val trees = Parser.interpretIndentation("test.vs", "answer is an int = 42").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("answer", Literal(42, Integer)))
    }

    "parse an assignment of a complex type to a variable with an explicit type" in {
      import Parser._

      val trees = Parser.interpretIndentation("test.vs", "answer is a color = color 255 0 0 255").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("answer", MethodCall("color", List(Literal(255, Integer), Literal(0, Integer), Literal(0, Integer), Literal(255, Integer)), null)))
    }

    "parse a parameter with parens" in {
      import Parser._
      val trees = Parser.interpretIndentation("test.vs", """
        |width is an int
        |height is an int
        |rect width height = 42
        |rect (width is an int = 800 ) (height is an int = 600 )""".stripMargin).map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(3) should be(Assignment("answer", MethodCall("color", List(Literal(255, Integer), Literal(0, Integer), Literal(0, Integer), Literal(255, Integer)), null)))
    }

    "parse an expression referring to a variable" in {
      import Parser._

      val trees = Parser.interpretIndentation("test.vs", "answer is an int = 42\nprintln answer").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(1) should be(MethodCall("println", Seq(Reference("answer", Integer)), null))
    }

    "parse a record definition" in {
      Parser("test.txt", """record color
|  r is any int
|  g is any int
|  b is any int
|  alpha is an int""".stripMargin) should be(Program(
  Seq.empty,
  Parser.Context(Map(
    "color" -> Record("color", Seq(
      Alias(Integer, "r", auto = true),
      Alias(Integer, "g", auto = true),
      Alias(Integer, "b", auto = true),
      Alias(Integer, "alpha", auto = false)
    ))), Map.empty)))
    }

    "parse the pixskillflut preamble" in {
      Parser.fromResource("/pixskillflut-preamble.vs")
    }

    import PixSkillFlutConstants._

    "parse a simple pixskillflut program" in {
      val preamble = Parser.fromResource("/pixskillflut-preamble.vs")
      val program = Parser.fromResource("/pixels.vs", preamble)
      val i = new Interpreter(program, pixskillflutBuiltins)
    }
  }
}