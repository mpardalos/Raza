package Raza 

import util.Try
import collection.mutable.ArrayBuffer
import reflect.ClassTag

sealed abstract class Stmt(val line: Int, val column: Int)
object Stmt {
  case class ExprStmt(expr: Expression, l: Int, c: Int) extends Stmt(l, c)
  case class Let(identifier: Expression.Identifier, value: Expression, l: Int, c: Int) extends Stmt(l, c)
  case class Print(value: Expression, l: Int, c: Int) extends Stmt(l, c)
}

sealed abstract class Expression(val line: Int, val column: Int)
object Expression {
  // Binary
  case class Addition(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Subtraction(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Multiplication(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Division(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Equal(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class NotEqual(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Less(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class Greater(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class LessEqual(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)
  case class GreaterEqual(left: Expression, right: Expression, l: Int, c: Int) extends Expression(l, c)

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

  def parse: List[Stmt] = {
    log("---Now Parsing---")
    index = 0
    val ast: ArrayBuffer[Stmt] = new ArrayBuffer()
    while (!peek.isInstanceOf[Token.EOF])
      ast += parseStmt
    log("---Parsing finished---")
    ast.toList
  }

  def parseExpression: Expression = {
    val p = parseEquality
    log("Expression parsed")
    p
  }

  def parsePrimary: Expression = {
    val token = next[Token]

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
        val rightParens = next[Token.RightParens]
        expr
      }
      case _ => throw new ParserException(token)
    })
    log(s"parsed primary ${ret}")
    ret
  }

  def parseUnary: Expression = peek match {
    case Token.Minus(l, c) => {
      next[Token]
      Expression.Minus(parseUnary, l, c)
    }
    case Token.Bang(l, c) => {
      next[Token]
      Expression.Not(parseUnary, l, c)
    }
    case _ => parsePrimary
  }

  def parseMultiplication: Expression = {
    val expr = parseUnary
    log(s"expr is: ${expr} in mul")
    
    peek match {
      case Token.Star(l, c) => {
        next[Token]
        Expression.Multiplication(expr, parseMultiplication, l, c)
      }
      case Token.Slash(l, c) => {
        next[Token]
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
        next[Token]
        Expression.Addition(expr, parseAddition, l, c)
      }
      case Token.Minus(l, c) => {
        next[Token]
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
        next[Token]
        Expression.Greater(expr, parseComparison, l, c)
      }
      case Token.Less(l, c) => {
        next[Token]
        Expression.Less(expr, parseComparison, l, c)
      }
      case Token.GreaterEqual(l, c) => {
        next[Token]
        Expression.GreaterEqual(expr, parseComparison, l, c)
      }
      case Token.LessEqual(l, c) => {
        next[Token]
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
        next[Token]
        Expression.Equal(expr, parseEquality, l, c)
      }
      case Token.BangEquals(l, c) => {
        next[Token]
        Expression.NotEqual(expr, parseEquality, l, c)
      }
      case Token.EOF(l, c) => {
        log("Saw EOF")
        expr
      }
      case _ => expr
    }
  }

  def parseStmt: Stmt = peek match {
    case Token.Print(l, c) => {
      next[Token.Print]
      val expr = parseExpression
      next[Token.Semicolon]
      Stmt.Print(expr, l, c)
    }
    case Token.Let(l, c) => {
      next[Token.Let]
      val id = next[Token.Identifier]
      next[Token.Equals]
      val expr = parseExpression
      next[Token.Semicolon] 
      Stmt.Let(Expression.Identifier(id.name, id.line, id.column), expr, l, c)
    }
    case Token(l, c) => {
      val expr = parseExpression
      next[Token.Semicolon]
      Stmt.ExprStmt(expr, l, c)
    }
  } 

  def checkAs[T](t: Token)(implicit tag: ClassTag[T]) = 
    Try(tag.runtimeClass.cast(t).asInstanceOf[T]) getOrElse {throw new ParserException(t)}

  def next[T <: Token](implicit tag: ClassTag[T]): T = {
    val token = allTokens(this.index)
    this.index += 1
    log(s"Consumed ${token}")

    checkAs[T](token)(tag)
  }

  private def peek: Token = allTokens(this.index)

  private def log(str: String) = if (loglevel) println(str) else Unit

  private def cantParse(token: Token) = throw new ParserException(token)
}
