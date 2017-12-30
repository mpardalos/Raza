package Raza

import scala.util.matching.Regex
import scala.collection.mutable

sealed abstract class TokenType
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
    TokenRule("^;".r,                      SEMICOLON),
    TokenRule("^\\[".r,                    LEFT_BRACKET),
    TokenRule("^\\]".r,                    RIGHT_BRACKET),
    TokenRule("^\\(".r,                    LEFT_PARENS),
    TokenRule("^\\)".r,                    RIGHT_PARENS),
    TokenRule("^\\{".r,                    LEFT_BRACE),
    TokenRule("^\\}".r,                    RIGHT_BRACE),
    TokenRule("^,".r,                      COMMA),
    TokenRule("^:".r,                      COLON),
    TokenRule("^\\+".r,                    PLUS),
    TokenRule("^-".r,                      MINUS),
    TokenRule("^/".r,                      SLASH),
    TokenRule("^\\*".r,                    STAR),
    TokenRule("^=>".r,                     FAT_ARROW),
    TokenRule("^==".r,                     EQUALS_EQUALS),
    TokenRule("^!=".r,                     BANG_EQUALS),
    TokenRule("^!".r,                      BANG),
    TokenRule("^>=".r,                     GREATER_EQUAL),
    TokenRule("^<=".r,                     LESS_EQUAL),
    TokenRule("^>".r,                      GREATER),
    TokenRule("^<".r,                      LESS),
    TokenRule("^=".r,                      EQUALS),
    TokenRule("^\\bvar\\b".r,              VAR),
    TokenRule("^\\blet\\b".r,              LET),
    TokenRule("^\\bor\\b".r,               OR),
    TokenRule("^\\band\\b".r,              AND),
    TokenRule("^\\bif\\b".r,               IF),
    TokenRule("^\\belse\\b".r,             ELSE),
    TokenRule("^\\bwhile\\b".r,            WHILE),
    TokenRule("^\\bnil\\b".r,              NIL),
    TokenRule("^\\breturn\\b".r,           RETURN),
    TokenRule("^\\bTrue\\b".r,             TRUE),
    TokenRule("^\\bFalse\\b".r,            FALSE),
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

  private def rule_whitespace(m: Regex.Match) = new Token(IGNORE, None, line, column)
  private def rule_newline(m: Regex.Match) = {
    this.line += 1
    this.column = 0
    new Token(IGNORE, None, line, column)
  }
  private def rule_string(m: Regex.Match) = new Token(STRING, 
    Some(m.toString.substring(1, m.toString.length-1)),
    line, column)
  private def rule_number(m: Regex.Match) = new Token(NUMBER, Some(m.toString), line, column)
  private def rule_identifier(m: Regex.Match) = new Token(IDENTIFIER, Some(m.toString), line, column)

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

    result.filter(_.tokenType != IGNORE).toList
  }

  def remainingSource = this.source.substring(this.current)
}




