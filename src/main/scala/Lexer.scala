package Raza

import scala.util.matching.Regex
import scala.collection.mutable

sealed abstract class Token(val line: Int, val column: Int) 
object Token {
  case class EOF(l: Int, c: Int) extends Token(l, c)
  case class Semicolon(l: Int, c: Int) extends Token(l, c)

  // 1-character
  case class LeftBracket(l: Int, c: Int) extends Token(l, c)
  case class RightBracket(l: Int, c: Int) extends Token(l, c)
  case class LeftParens(l: Int, c: Int) extends Token(l, c)
  case class RightParens(l: Int, c: Int) extends Token(l, c)
  case class LeftBrace(l: Int, c: Int) extends Token(l, c)
  case class RightBrace(l: Int, c: Int) extends Token(l, c)
  case class Comma(l: Int, c: Int) extends Token(l, c)
  case class Colon(l: Int, c: Int) extends Token(l, c)
  case class Equals(l: Int, c: Int) extends Token(l, c)
  case class Plus(l: Int, c: Int) extends Token(l, c)
  case class Minus(l: Int, c: Int) extends Token(l, c)
  case class Slash(l: Int, c: Int) extends Token(l, c)
  case class Star(l: Int, c: Int) extends Token(l, c)

  // Symbols
  case class FatArrow(l: Int, c: Int) extends Token(l, c)
  case class Bang(l: Int, c: Int) extends Token(l, c)
  case class BangEquals(l: Int, c: Int) extends Token(l, c)
  case class EqualsEquals(l: Int, c: Int) extends Token(l, c)
  case class Greater(l: Int, c: Int) extends Token(l, c)
  case class Less(l: Int, c: Int) extends Token(l, c)
  case class GreaterEqual(l: Int, c: Int) extends Token(l, c)
  case class LessEqual(l: Int, c: Int) extends Token(l, c)

  // Keywords
  case class Var(l: Int, c: Int) extends Token(l, c)
  case class Let(l: Int, c: Int) extends Token(l, c)
  case class Or(l: Int, c: Int) extends Token(l, c)
  case class And(l: Int, c: Int) extends Token(l, c)
  case class If(l: Int, c: Int) extends Token(l, c)
  case class Else(l: Int, c: Int) extends Token(l, c)
  case class While(l: Int, c: Int) extends Token(l, c)
  case class Nil(l: Int, c: Int) extends Token(l, c)
  case class Return(l: Int, c: Int) extends Token(l, c)
  case class True(l: Int, c: Int) extends Token(l, c)
  case class False(l: Int, c: Int) extends Token(l, c)
  case class Fun(l: Int, c: Int) extends Token(l, c)

  // Literals
  case class Identifier(name: String, l: Int, c: Int) extends Token(l, c)
  case class Str(value: String, l: Int, c: Int) extends Token(l, c)
  case class WholeNumber(value: String, l: Int, c: Int) extends Token(l, c)
  case class DecimalNumber(value: String, l: Int, c: Int) extends Token(l, c)

  def unapply(token: Token): Option[(Int, Int)] = Some((token.line, token.column))
}

  case class LexerException(val line: Int, val column: Int) extends Exception

  class Lexer(source: String) {
    private val rules: List[(Regex, (Regex.Match => Option[Token]))] = List(
      ("^;".r,                  m => Some(new Token.Semicolon(line, column))),
      ("^\\[".r,                m => Some(new Token.LeftBracket(line, column))),
      ("^\\]".r,                m => Some(new Token.RightBracket(line, column))),
      ("^\\(".r,                m => Some(new Token.LeftParens(line, column))),
      ("^\\)".r,                m => Some(new Token.RightParens(line, column))),
      ("^\\{".r,                m => Some(new Token.LeftBrace(line, column))),
      ("^\\}".r,                m => Some(new Token.RightBrace(line, column))),
      ("^,".r,                  m => Some(new Token.Comma(line, column))),
      ("^:".r,                  m => Some(new Token.Colon(line, column))),
      ("^\\+".r,                m => Some(new Token.Plus(line, column))),
      ("^-".r,                  m => Some(new Token.Minus(line, column))),
      ("^/".r,                  m => Some(new Token.Slash(line, column))),
      ("^\\*".r,                m => Some(new Token.Star(line, column))),
      ("^=>".r,                 m => Some(new Token.FatArrow(line, column))),
      ("^==".r,                 m => Some(new Token.EqualsEquals(line, column))),
      ("^!=".r,                 m => Some(new Token.BangEquals(line, column))),
      ("^!".r,                  m => Some(new Token.Bang(line, column))),
      ("^>=".r,                 m => Some(new Token.GreaterEqual(line, column))),
      ("^<=".r,                 m => Some(new Token.LessEqual(line, column))),
      ("^>".r,                  m => Some(new Token.Greater(line, column))),
      ("^<".r,                  m => Some(new Token.Less(line, column))),
      ("^=".r,                  m => Some(new Token.Equals(line, column))),
      ("^\\bvar\\b".r,          m => Some(new Token.Var(line, column))),
      ("^\\blet\\b".r,          m => Some(new Token.Let(line, column))),
      ("^\\bor\\b".r,           m => Some(new Token.Or(line, column))),
      ("^\\band\\b".r,          m => Some(new Token.And(line, column))),
      ("^\\bif\\b".r,           m => Some(new Token.If(line, column))),
      ("^\\belse\\b".r,         m => Some(new Token.Else(line, column))),
      ("^\\bwhile\\b".r,        m => Some(new Token.While(line, column))),
      ("^\\bnil\\b".r,          m => Some(new Token.Nil(line, column))),
      ("^\\breturn\\b".r,       m => Some(new Token.Return(line, column))),
      ("^\\bTrue\\b".r,         m => Some(new Token.True(line, column))),
      ("^\\bFalse\\b".r,        m => Some(new Token.False(line, column))),
      ("^\\bfun\\b".r,          m => Some(new Token.Fun(line, column))),
      ("^ ".r,                  m => None),
      ("^(\\n|#.*)".r,       m => {
        this.line += 1
        this.column = 0
        None
      }),
      ("^\".*?\"".r,            m => Some(new Token.Str(
        m.toString.substring(1, m.toString.length-1), line, column))),
      ("^\\d+(?!\\w|\\.)".r,        m => Some(new Token.WholeNumber(m.toString, line, column))),
      ("^\\d+\\.\\d+(?!\\w|\\.)".r, m => Some(new Token.DecimalNumber(m.toString, line, column))),
      ("^\\b(?!\\d)\\w+\\b".r,  m => Some(new Token.Identifier(m.toString, line, column)))
    )

    private var current: Int = 0
    private var line: Int = 0
    private var column: Int = 0

    def tokens: List[Token] = {
      var result: mutable.ListBuffer[Option[Token]] = mutable.ListBuffer()

      while (this.current < this.source.length) {
        val rule: Option[(Regex, Regex.Match => Option[Token])] = 
          rules.find(r => r._1.findFirstMatchIn(remainingSource).isDefined)

        result += (rule match {
          case Some((regex, func)) => {
            // We have asserted that this is Some in the find above
            val m = regex.findFirstMatchIn(remainingSource).get

            current += m.end
            column += m.end

            func(m)
          }
          case None => throw new LexerException(line, column)
        })
      }
      result += Some(Token.EOF(line, column))

      result.flatten.toList
    }

    def remainingSource = this.source.substring(this.current)
  }




