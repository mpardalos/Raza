package Raza

import scala.util.matching.Regex
import scala.collection.mutable

sealed abstract class Token(line: Int, column: Int) 
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

  // Literals
  case class Identifier(value: String, l: Int, c: Int) extends Token(l, c)
  case class Str(value: String, l: Int, c: Int) extends Token(l, c)
  case class Number(value: String, line: Int, column: Int) extends Token(l, c)

  // Ignored Tokens
  case class Ignore(l: Int, c: Int) extends Token(l, c)
}

  case class LexerException(val line: Int, val column: Int) extends Exception

  class Lexer(source: String) {
    private val rules: List[(Regex, (Regex.Match => Token))] = List(
      ("^;".r,                      m => new Token.Semicolon(line, column)),
      ("^\\[".r,                    m => new Token.LeftBracket(line, column)),
      ("^\\]".r,                    m => new Token.RightBracket(line, column)),
      ("^\\(".r,                    m => new Token.LeftParens(line, column)),
      ("^\\)".r,                    m => new Token.RightParens(line, column)),
      ("^\\{".r,                    m => new Token.LeftBrace(line, column)),
      ("^\\}".r,                    m => new Token.RightBrace(line, column)),
      ("^,".r,                      m => new Token.Comma(line, column)),
      ("^:".r,                      m => new Token.Colon(line, column)),
      ("^\\+".r,                    m => new Token.Plus(line, column)),
      ("^-".r,                      m => new Token.Minus(line, column)),
      ("^/".r,                      m => new Token.Slash(line, column)),
      ("^\\*".r,                    m => new Token.Star(line, column)),
      ("^=>".r,                     m => new Token.FatArrow(line, column)),
      ("^==".r,                     m => new Token.EqualsEquals(line, column)),
      ("^!=".r,                     m => new Token.BangEquals(line, column)),
      ("^!".r,                      m => new Token.Bang(line, column)),
      ("^>=".r,                     m => new Token.GreaterEqual(line, column)),
      ("^<=".r,                     m => new Token.LessEqual(line, column)),
      ("^>".r,                      m => new Token.Greater(line, column)),
      ("^<".r,                      m => new Token.Less(line, column)),
      ("^=".r,                      m => new Token.Equals(line, column)),
      ("^\\bvar\\b".r,              m => new Token.Var(line, column)),
      ("^\\blet\\b".r,              m => new Token.Let(line, column)),
      ("^\\bor\\b".r,               m => new Token.Or(line, column)),
      ("^\\band\\b".r,              m => new Token.And(line, column)),
      ("^\\bif\\b".r,               m => new Token.If(line, column)),
      ("^\\belse\\b".r,             m => new Token.Else(line, column)),
      ("^\\bwhile\\b".r,            m => new Token.While(line, column)),
      ("^\\bnil\\b".r,              m => new Token.Nil(line, column)),
      ("^\\breturn\\b".r,           m => new Token.Return(line, column)),
      ("^\\bTrue\\b".r,             m => new Token.True(line, column)),
      ("^\\bFalse\\b".r,            m => new Token.False(line, column)),
      ("^ ".r,                      m => new Token.Ignore(line, column)),
      ("^(\\n|\\#.*$)".r,           m => {
        this.line += 1
        this.column = 0
        new Token.Ignore(line, column)
      }),
    ("^\".*?\"".r,                m => 
        new Token.Str(m.toString.substring(1, m.toString.length-1), line, column)),
      ("^\\d+(\\.\\d+)?(?!\\w)".r,  m => new Token.Number(m.toString, line, column)),
      ("^\\b(?!\\d)\\w+\\b".r,      m => new Token.Identifier(m.toString, line, column))
    )

    private var current: Int = 0
    private var line: Int = 0
    private var column: Int = 0

    def tokens: List[Token] = {
      var result: mutable.ListBuffer[Token] = mutable.ListBuffer()

      while (this.current < this.source.length) {
        val rule: Option[(Regex, Regex.Match => Token)] = 
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

      result.filter(!_.isInstanceOf[Token.Ignore]).toList
    }

    def remainingSource = this.source.substring(this.current)
  }




