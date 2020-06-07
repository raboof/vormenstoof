package vormenstoof

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "The parser" should {
    import Parser._

    "parse 3 levels of indentation into the correct tree" in {
      Parser.interpretIndentation("test.txt", """
        |foo
        |  bar
        |    baz""".stripMargin) match {
        case List(Tree("foo", Seq(Tree("bar", Seq(Tree("baz", _, _)), _)), _)) => // ok
      }
    }

    "parse returning to the previous level of indentation" in {
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
      val tree = Parser.tokenize(Tree("r is an int", Seq.empty, Location.unknown))
      tree.top should be ("r")
      tree.children.size should be(2)
      tree.children(0).top should be("is an")
      tree.children(1).top should be("int")
    }

    "tokenize parens as separate subtrees" in {
      val trees = Parser.interpretIndentation("test.txt", """
          |foo quux
          |  bar (pom piedom)
          |    baz
          |  qux""".stripMargin).map(Parser.tokenize)
      trees match {
        case List(
        Tree("foo", List(
          Tree("quux", List(), _),
          Tree("bar", List(
            Tree("pom", List(Tree("piedom", _, _)), _),
            Tree("baz", _, _)), _),
          Tree("qux", _, _)), _
          )) => // ok
      }
    }

    "parse a named variable of primitive type" in {
      val tree = Parser.tokenize(Tree("r is an int", Seq.empty, Location.unknown))
      Parser.parseAliasOpt(tree, Context.empty) should be(Some(Alias(Integer, "r", auto = false)))
      Parser.parseType(tree, Context.empty) should be(Alias(Integer, "r", auto = false))
    }

    "parse a named variable of composite type" in {
      val compositeType = Record("color", Seq.empty)
      val tree = Parser.tokenize(Tree("r is a color", Seq.empty, Location.unknown))
      Parser.parseAliasOpt(tree, Context(compositeType)) should be(Some(Alias(compositeType, "r", auto = false)))
    }

    "parse an assignment of a literal to a primitive variable" in {
      val trees = Parser.interpretIndentation("test.vs", "int = 42").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("int", Literal(42, Integer), Integer))
    }

    "parse an assignment of a literal to a variable with an explicit type" in {
      val trees = Parser.interpretIndentation("test.vs", "answer is an int = 42").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("answer", Literal(42, Integer), Integer))
    }

    "parse an assignment of a complex type to a variable with an explicit type" in {
      val trees = Parser.interpretIndentation("test.vs", "answer is a color = color 255 0 0 255").map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      segments(0) should be(Assignment("answer", MethodCall("color", List(Literal(255, Integer), Literal(0, Integer), Literal(0, Integer), Literal(255, Integer)), null), null))
    }

    "parse a parameter with parens" in {
      val trees = Parser.interpretIndentation("test.vs", """
        |width is an int
        |height is an int
        |rect width height = 42
        |rect (width = 800) (height = 600)""".stripMargin).map(Parser.tokenize)
      val (_, segments) = Parser.parseSegments(trees, Context.empty)
      val MethodCall("rect", params, _) = segments(3)
      // TODO once we support type scoping, the assignment type likely should not be
      // 'Integer' but the method-local 'width' and 'height' types
      params should be(List(Assignment("width", Literal(800, Integer), Integer), Assignment("height", Literal(600, Integer), Integer)))
    }

    "parse an expression referring to a variable" in {
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