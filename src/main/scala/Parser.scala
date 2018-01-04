package Raza 

object `package` {
  // Placeholder until other syntactic constructs are implemented
  type AST = Expression
}

sealed abstract class Expression(val line: Int, val column: Int)
object Expression {
  // Binary
  case class Addition(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Subtraction(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Multiplication(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Division(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Equal(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class NotEqual(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Less(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class Greater(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class LessEqual(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)
  case class GreaterEqual(left: Expression, right: Expression, ln: Int, c: Int) extends Expression(ln, c)

  // Unary
  case class Not(expr: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Minus(expr: Expression, l: Int, c: Int) extends Expression(l, c)

  // Literals
  case class Str(str: String, l: Int, c: Int) extends Expression(l, c)
  case class Integer(num: Int, l: Int, c: Int) extends Expression(l, c)
  case class Decimal(num: Double, l: Int, c: Int) extends Expression(l, c)
  case class Identifier(name: String, l: Int, c: Int) extends Expression(l, c)
  case class True(l: Int, c: Int) extends Expression(l, c)
  case class False(l: Int, c: Int) extends Expression(l, c)
  case class Nil(l: Int, c: Int) extends Expression(l, c)
}

case class ParserException(val token: Token) extends Exception

class Parser(allTokens: List[Token], val loglevel: Boolean = false) {
  private var index: Int = 0

  def parse: AST = {
    log("---Now Parsing---")
    index = 0
    val p = parseExpression
    log("---Parsing finished---")
    p
  }

  def parseExpression: Expression = {
    val p = parseEquality
    log("Expression parsed")
    p
  }

  def parsePrimary: Expression = {
    val token = nextToken

    val ret: Expression = (token match {
      case Token.Str(str, l, c) => Expression.Str(str, l, c)
      case Token.Identifier(name, l, c) => Expression.Identifier(name, l, c)
      case Token.WholeNumber(numStr, l, c) => Expression.Integer(numStr.toInt, l, c)
      case Token.DecimalNumber(numStr, l, c) => Expression.Decimal(numStr.toDouble, l, c)
      case Token.True(l, c) => Expression.True(l, c)
      case Token.False(l, c) => Expression.False(l, c)
      case Token.Nil(l, c) => Expression.Nil(l, c)

      case Token.LeftParens(l, c) => {
        val expr = parseExpression
        val rightParens = nextToken
        if (!rightParens.isInstanceOf[Token.RightParens])
          throw new ParserException(token)

        expr
      }

      case _ => throw new ParserException(token)
    })
    log(s"parsed primary ${ret}")
    ret
  }

  def parseUnary: Expression = peek match {
    case Token.Minus(l, c) => {
      nextToken
      Expression.Minus(parseUnary, l, c)
    }
    case Token.Bang(l, c) => {
      nextToken
      Expression.Not(parseUnary, l, c)
    }
    case _ => parsePrimary
  }

  def parseMultiplication: Expression = {
    val expr = parseUnary
    log(s"expr is: ${expr} in mul")
    
    peek match {
      case Token.Star(l, c) => {
        nextToken
        Expression.Multiplication(expr, parseMultiplication, l, c)
      }
      case Token.Slash(l, c) => {
        nextToken
        Expression.Division(expr, parseMultiplication, l, c)
      }
      case _ => expr
    }
  }

  def parseAddition: Expression = {
    val expr = parseMultiplication
    log(s"expr is: ${expr} in add")

    peek match {
      case Token.Plus(l, c) => {
        nextToken
        Expression.Addition(expr, parseAddition, l, c)
      }
      case Token.Minus(l, c) => {
        nextToken
        Expression.Subtraction(expr, parseAddition, l, c)
      }
      case _ => expr
    }
  }

  def parseComparison: Expression = {
    val expr = parseAddition
    log(s"expr is: ${expr} in comparison")

    peek match {
      case Token.Greater(l, c) => {
        nextToken
        Expression.Greater(expr, parseComparison, l, c)
      }
      case Token.Less(l, c) => {
        nextToken
        Expression.Less(expr, parseComparison, l, c)
      }
      case Token.GreaterEqual(l, c) => {
        nextToken
        Expression.GreaterEqual(expr, parseComparison, l, c)
      }
      case Token.LessEqual(l, c) => {
        nextToken
        Expression.LessEqual(expr, parseComparison, l, c)
      }
      case _ => expr
    }
  }
  
  def parseEquality: Expression = {
    val expr = parseComparison
    log(s"expr is: ${expr} in equality")

    peek match {
      case Token.EqualsEquals(l, c) => {
        nextToken
        Expression.Equal(expr, parseEquality, l, c)
      }
      case Token.BangEquals(l, c) => {
        nextToken
        Expression.NotEqual(expr, parseEquality, l, c)
      }
      case Token.EOF(l, c) => {
        log("Saw EOF")
        expr
      }
      case _ => expr
    }
  }

  private def nextToken: Token = {
    val t = allTokens(this.index)
    log(s"Consumed ${t}")
    this.index += 1
    t
  }

  private def peek: Token = allTokens(this.index)

  private def log(str: String) = if (loglevel) println(str) else Unit
}
