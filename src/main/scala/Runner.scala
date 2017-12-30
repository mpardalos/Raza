package Raza

import scala.io.Source

object Main extends App {
  val source = Source.fromFile(args(0)).mkString

  try {
    val lexer = new Lexer(source)
    println(lexer.tokens.mkString("\n"))
  } catch {
    case LexerException(line, column) => {
      println(
        s"Lexing error at line ${line+1}:\n" +
        source.split("\n")(line) + "\n" +
        " " * column + "^"
      )
    }
  }
}
