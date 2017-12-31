package Raza

import scala.util.matching.Regex
import scala.collection.mutable

sealed abstract class TokenType
object TokenType {
  case object EOF extends TokenType
  case object SEMICOLON extends TokenType

  // 1-character
  case object LEFT_BRACKET extends TokenType
  case object RIGHT_BRACKET extends TokenType
  case object LEFT_PARENS extends TokenType
  case object RIGHT_PARENS extends TokenType
  case object LEFT_BRACE extends TokenType
  case object RIGHT_BRACE extends TokenType
  case object COMMA extends TokenType
  case object COLON extends TokenType
  case object EQUALS extends TokenType
  case object PLUS extends TokenType
  case object MINUS extends TokenType
  case object SLASH extends TokenType
  case object STAR extends TokenType

  // Symbols
  case object FAT_ARROW extends TokenType
  case object BANG extends TokenType
  case object BANG_EQUALS extends TokenType
  case object EQUALS_EQUALS extends TokenType
  case object GREATER extends TokenType
  case object LESS extends TokenType
  case object GREATER_EQUAL extends TokenType
  case object LESS_EQUAL extends TokenType

  // Keywords
  case object VAR extends TokenType
  case object LET extends TokenType
  case object OR extends TokenType
  case object AND extends TokenType
  case object IF extends TokenType
  case object ELSE extends TokenType
  case object WHILE extends TokenType
  case object NIL extends TokenType
  case object RETURN extends TokenType
  case object TRUE extends TokenType
  case object FALSE extends TokenType

  // Literals
  case object IDENTIFIER extends TokenType
  case object STRING extends TokenType
  case object NUMBER extends TokenType

  // Ignored Tokens
  case object IGNORE extends TokenType
}

class Token(val tokenType: TokenType, val value: Option[String], val line: Int, val column: Int) {
  override def toString: String = tokenType + (value match {
    case Some(v) => "(" + v + ")"
    case None => ""
  })
}

case class LexerException(val line: Int, val column: Int) extends Exception

class Lexer(source: String) {
  private sealed abstract class Rule(val regex: Regex)
  private case class TokenRule(val r: Regex, val tokenType: TokenType) extends Rule(r)
  private case class MethodRule(val r: Regex, val func: Regex.Match => Token) extends Rule(r)

  private val rules: List[Rule] = List(
    TokenRule("^;".r,                      TokenType.SEMICOLON),
    TokenRule("^\\[".r,                    TokenType.LEFT_BRACKET),
    TokenRule("^\\]".r,                    TokenType.RIGHT_BRACKET),
    TokenRule("^\\(".r,                    TokenType.LEFT_PARENS),
    TokenRule("^\\)".r,                    TokenType.RIGHT_PARENS),
    TokenRule("^\\{".r,                    TokenType.LEFT_BRACE),
    TokenRule("^\\}".r,                    TokenType.RIGHT_BRACE),
    TokenRule("^,".r,                      TokenType.COMMA),
    TokenRule("^:".r,                      TokenType.COLON),
    TokenRule("^\\+".r,                    TokenType.PLUS),
    TokenRule("^-".r,                      TokenType.MINUS),
    TokenRule("^/".r,                      TokenType.SLASH),
    TokenRule("^\\*".r,                    TokenType.STAR),
    TokenRule("^=>".r,                     TokenType.FAT_ARROW),
    TokenRule("^==".r,                     TokenType.EQUALS_EQUALS),
    TokenRule("^!=".r,                     TokenType.BANG_EQUALS),
    TokenRule("^!".r,                      TokenType.BANG),
    TokenRule("^>=".r,                     TokenType.GREATER_EQUAL),
    TokenRule("^<=".r,                     TokenType.LESS_EQUAL),
    TokenRule("^>".r,                      TokenType.GREATER),
    TokenRule("^<".r,                      TokenType.LESS),
    TokenRule("^=".r,                      TokenType.EQUALS),
    TokenRule("^\\bvar\\b".r,              TokenType.VAR),
    TokenRule("^\\blet\\b".r,              TokenType.LET),
    TokenRule("^\\bor\\b".r,               TokenType.OR),
    TokenRule("^\\band\\b".r,              TokenType.AND),
    TokenRule("^\\bif\\b".r,               TokenType.IF),
    TokenRule("^\\belse\\b".r,             TokenType.ELSE),
    TokenRule("^\\bwhile\\b".r,            TokenType.WHILE),
    TokenRule("^\\bnil\\b".r,              TokenType.NIL),
    TokenRule("^\\breturn\\b".r,           TokenType.RETURN),
    TokenRule("^\\bTrue\\b".r,             TokenType.TRUE),
    TokenRule("^\\bFalse\\b".r,            TokenType.FALSE),
    MethodRule("^ ".r,                     rule_whitespace),
    MethodRule("^\\n".r,                   rule_newline),
    MethodRule("^\\#.*$".r,                rule_newline), // Ignore comments
    MethodRule("^\".*?\"".r,               rule_string),
    MethodRule("^\\d+(\\.\\d+)?(?!\\w)".r, rule_number),
    MethodRule("^\\b(?!\\d)\\w+\\b".r,     rule_identifier)
  )

  private var current: Int = 0
  private var line: Int = 0
  private var column: Int = 0

  private def rule_whitespace(m: Regex.Match) = new Token(TokenType.IGNORE, None, line, column)
  private def rule_newline(m: Regex.Match) = {
    this.line += 1
    this.column = 0
    new Token(TokenType.IGNORE, None, line, column)
  }
  private def rule_string(m: Regex.Match) = new Token(TokenType.STRING, 
    Some(m.toString.substring(1, m.toString.length-1)),
    line, column)
  private def rule_number(m: Regex.Match) = new Token(TokenType.NUMBER, Some(m.toString), line, column)
  private def rule_identifier(m: Regex.Match) = new Token(TokenType.IDENTIFIER, Some(m.toString), line, column)

  def tokens: List[Token] = {
    var result: mutable.ListBuffer[Token] = mutable.ListBuffer()

    while (this.current < this.source.length) {
      val rule = rules.find(_.regex.findFirstMatchIn(remainingSource).isDefined)
      
      result += (rule match {
        case Some(TokenRule(regex, tokenType)) => new Token(tokenType, None, line, column)
        case Some(MethodRule(regex, func)) => func(regex.findFirstMatchIn(remainingSource).get)
        case None => throw new LexerException(line, column)
      })

      rule.map(r => {
        val m = r.regex.findFirstMatchIn(remainingSource).get
        current += m.end
        column += m.end
      }) 
      
    }

    result.filter(_.tokenType != TokenType.IGNORE).toList
  }

  def remainingSource = this.source.substring(this.current)
}




