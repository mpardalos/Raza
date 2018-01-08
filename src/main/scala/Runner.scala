package Raza

import scala.io.Source

object Main extends App {
  val source = Source.fromFile(args(0)).mkString
  def sourceLine(line: Int): String = (source + " ").split("\n")(line)

  try {
    val lexer = new Lexer(source)
    val tokens = lexer.tokens
    // println(tokens)

    val parser = new Parser(tokens)
    val ast = parser.parse
    // println(ast)

    val interpreter = new Interpreter(ast)
    interpreter.interpretAll

  } catch {
    case LexerException(line, column) => {
      println(
        s"Lexing error at line ${line+1}:\n" +
        sourceLine(line) + "\n" +
        " " * column + "^"
      )
    }
    case ParserException(token) => {
      println(
        s"Could not parse ${token} at line ${token.line+1}:\n" +
        sourceLine(token.line) + "\n" +
        " " * token.column + "^\n"
      )
    }
    case RazaRuntimeException(line, column, msg) => {
      println(
        s"Runtime Error at line ${line}:\n" +
        sourceLine(line) + "\n" +
        " " * column + "^\n" +
        msg
      )
    }
  }
}
