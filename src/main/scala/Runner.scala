package Raza

import scala.io.Source
import scala.util.Try

object Main extends App {
  val usage = """
    Usage: raza [--lex|--parse] file.rz
  """

  case class Options(action: Action, filename: String)
  abstract class Action
  case object Lex extends Action
  case object Parse extends Action
  case object Run extends Action

  def parseArgs(args: List[String]): Option[Options] = args match {
    case "--lex" :: filename :: Nil => Some(Options(Lex, filename))
    case "--parse" :: filename :: Nil => Some(Options(Parse, filename))
    case filename :: Nil => Some(Options(Run, filename))
    case _ => None
  }

  parseArgs(args.toList) match {
    case Some(Options(Lex, fname)) =>{
      val src = load(fname)
      lex(src).map(println) recover errorHandler(src)
    }
    case Some(Options(Parse, fname)) => {
      val src = load(fname)
      parse(src).map(println) recover errorHandler(src)
    }
    case Some(Options(Run, fname)) => {
      val src = load(fname)
      run(src) recover errorHandler(src)
    }
    case None => println(usage)
  }

  def lex(src: String): Try[List[Token]] = Try (new Lexer(src).tokens) 
  def parse(src: String): Try[Block] = lex(src).map(new Parser(_).parse) 
  def run(src: String): Try[Unit] = parse(src).map(Interpreter.execBlock(_)) 
     
  def sourceLine(source: String, line: Int): String = (source + " ").split("\n")(line)
  def load(file: String) = Source.fromFile(file).mkString

  def errorHandler(src: String): PartialFunction[Throwable, Unit] = {
    case LexerException(line, column) => println(
      s"Lexing error at line ${line+1}:\n" +
      sourceLine(src, line) + "\n" +
      " " * column + "^"
    );
    case ParserException(token) => println(
      s"Could not parse ${token} at line ${token.line+1}:\n" +
      sourceLine(src, token.line) + "\n" +
      " " * token.column + "^\n"
    );
    case RazaRuntimeException(line, column, msg) => println(
      s"Runtime Error at line ${line}:\n" +
      sourceLine(src, line) + "\n" +
      " " * column + "^\n" +
      msg
    )
  }
  
}
