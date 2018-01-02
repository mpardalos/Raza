package Raza 

sealed abstract class Expression
object Expression {
  sealed abstract class Binary(left: Expression, right: Expression) extends Expression 
  case class Addition(l: Expression, r: Expression) extends Binary(l, r)
  case class Subtraction(l: Expression, r: Expression) extends Binary(l, r)
  case class Multiplication(l: Expression, r: Expression) extends Binary(l, r)
  case class Division(l: Expression, r: Expression) extends Binary(l, r)
  case class Equal(l: Expression, r: Expression) extends Binary(l, r)
  case class NotEqual(l: Expression, r: Expression) extends Binary(l, r)
  case class Less(l: Expression, r: Expression) extends Binary(l, r)
  case class Greater(l: Expression, r: Expression) extends Binary(l, r)
  case class LessEqual(l: Expression, r: Expression) extends Binary(l, r)
  case class GreaterEqual(l: Expression, r: Expression) extends Binary(l, r)

  sealed abstract class Unary(expr: Expression) extends Expression
  case class Invert(e: Expression) extends Unary(e)
  case class Negate(e: Expression) extends Unary(e)

  sealed abstract class Literal extends Expression
  case class Str(str: String) extends Literal
  case class Number[T: Numeric](num: T) extends Literal
  case class Identifier(name: String) extends Literal
  case object True extends Literal
  case object False extends Literal
  case object Nil extends Literal

}

case class ParserException(val token: Token) extends Exception

class Parser(allTokens: List[Token], val loglevel: Boolean = false) {
  private var index: Int = 0

  def parse: Expression = {
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

    val ret = (token match {
      case Token.Str(str, _, _) => Expression.Str(str)
      case Token.Identifier(name, _, _) => Expression.Identifier(name)
      case Token.WholeNumber(numStr, _, _) => Expression.Number(numStr.toInt)
      case Token.DecimalNumber(numStr, _, _) => Expression.Number(numStr.toDouble)
      case Token.True(_, _) => Expression.True
      case Token.False(_, _) => Expression.False
      case Token.Nil(_, _) => Expression.Nil

      case Token.LeftParens(_, _) => {
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
    case Token.Minus(_, _) => {
      nextToken
      Expression.Invert(parseUnary)
    }
    case Token.Bang(_, _) => {
      nextToken
      Expression.Invert(parseUnary)
    }
    case _ => parsePrimary
  }

  def parseMultiplication: Expression = {
    val expr = parseUnary
    log(s"expr is: ${expr} in mul")
    
    peek match {
      case Token.Star(_, _) => {
        nextToken
        Expression.Multiplication(expr, parseMultiplication)
      }
      case Token.Slash(_, _) => {
        nextToken
        Expression.Division(expr, parseMultiplication)
      }
      case _ => expr
    }
  }

  def parseAddition: Expression = {
    val expr = parseMultiplication
    log(s"expr is: ${expr} in add")

    peek match {
      case Token.Plus(_, _) => {
        nextToken
        Expression.Addition(expr, parseAddition)
      }
      case Token.Minus(_, _) => {
        nextToken
        Expression.Subtraction(expr, parseAddition)
      }
      case _ => expr
    }
  }

  def parseComparison: Expression = {
    val expr = parseAddition
    log(s"expr is: ${expr} in comparison")

    peek match {
      case Token.Greater(_, _) => {
        nextToken
        Expression.Greater(expr, parseComparison)
      }
      case Token.Less(_, _) => {
        nextToken
        Expression.Less(expr, parseComparison)
      }
      case Token.GreaterEqual(_, _) => {
        nextToken
        Expression.GreaterEqual(expr, parseComparison)
      }
      case Token.LessEqual(_, _) => {
        nextToken
        Expression.LessEqual(expr, parseComparison)
      }
      case _ => expr
    }
  }
  
  def parseEquality: Expression = {
    val expr = parseComparison
    log(s"expr is: ${expr} in equality")

    peek match {
      case Token.EqualsEquals(_, _) => {
        nextToken
        Expression.Equal(expr, parseEquality)
      }
      case Token.BangEquals(_, _) => {
        nextToken
        Expression.NotEqual(expr, parseEquality)
      }
      case Token.EOF(_, _) => {
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
