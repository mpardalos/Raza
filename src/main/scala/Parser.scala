package Raza 

import util.Try
import collection.mutable.ArrayBuffer
import reflect.ClassTag

object `package` {
  type Block = List[Stmt]
}

sealed abstract class Stmt(val line: Int, val column: Int)
object Stmt {
  case class ExprStmt(expr: Expression, l: Int, c: Int) extends Stmt(l, c)

  sealed abstract class Declaration(val identifier: Expression.Identifier, val value: Expression, 
                                    override val line: Int,
                                    override val column: Int) extends Stmt(line, column)
  case class Let(id: Expression.Identifier, v: Expression, l: Int, c: Int) extends Declaration(id, v, l, c)
  case class Var(id: Expression.Identifier, v: Expression, l: Int, c: Int) extends Declaration(id, v, l, c)
  object Declaration {
    def unapply(d: Declaration): Option[(Expression.Identifier, Expression, Int, Int)] = 
      Some((d.identifier, d.value, d.line, d.column))
  }

  case class Print(value: Expression, l: Int, c: Int) extends Stmt(l, c)
  case class If(condition: Expression, ifBlock: Block, elseBlock: Block, l: Int, c: Int) extends Stmt(l, c)
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
  case class Call(callee: Expression, args: List[Expression], l: Int, c: Int) extends Expression(l, c)

  // Literals
  case class Str(str: String, l: Int, c: Int) extends Expression(l, c)
  case class Integer(num: Int, l: Int, c: Int) extends Expression(l, c)
  case class Decimal(num: Double, l: Int, c: Int) extends Expression(l, c)
  case class Identifier(name: String, l: Int, c: Int) extends Expression(l, c)
  case class True(l: Int, c: Int) extends Expression(l, c)
  case class False(l: Int, c: Int) extends Expression(l, c)
  case class Nil_(l: Int, c: Int) extends Expression(l, c)

  case class FunctionDef(args: List[Identifier], body: Block, l: Int, c: Int) extends Expression(l, c)
}

case class ParserException(val token: Token) extends Exception

class Parser(allTokens: List[Token], val loglevel: Boolean = false) {
  private var index: Int = 0

  def parse: Block = {
    log("---Now Parsing---")
    index = 0
    val ast: ArrayBuffer[Stmt] = new ArrayBuffer()
    while (!peek.isInstanceOf[Token.EOF])
      ast += parseStmt
    log("---Parsing finished---")
    ast.toList
  }

  /* 
   * expression -> equality 
   */
  def parseExpression: Expression = {
    val p = parseEquality
    log("Expression parsed")
    p
  }

  /* 
   * primary -> STRING | IDENTIFIER | WHOLENUMBER | DECIMALNUMBER 
   *          | "True" | "False" | "Nil" | "(" expression ")" | functionDef
   *
   * functionDef -> "fun" "(" args ")" "=>" (expression | block)
   */
  def parsePrimary: Expression = {
    val token = next[Token]

    val ret: Expression = (token match {
      case Token.Str(str, l, c) => Expression.Str(str, l, c)
      case Token.Identifier(name, l, c) => Expression.Identifier(name, l, c)
      case Token.WholeNumber(numStr, l, c) => Expression.Integer(numStr.toInt, l, c)
      case Token.DecimalNumber(numStr, l, c) => Expression.Decimal(numStr.toDouble, l, c)
      case Token.True(l, c) => Expression.True(l, c)
      case Token.False(l, c) => Expression.False(l, c)
      case Token.Nil(l, c) => Expression.Nil_(l, c)
      
      case Token.Fun(l, c) => {
        next[Token.LeftParens]
        val args = if (!peek.isInstanceOf[Token.RightParens]) parseArgNames else List()
        next[Token.RightParens]
        next[Token.FatArrow]
        
        val body = if (peek.isInstanceOf[Token.LeftBrace]) parseBlock 
          else {
            val e = parseExpression
            List(new Stmt.ExprStmt(e, e.line, e.column))
          }
        
        Expression.FunctionDef(args, body, l, c)
      }

      case Token.LeftParens(l, c) => {
        val expr = parseExpression
        val rightParens = next[Token.RightParens]
        expr
      }
      case _ => cantParse(token)
    })
    log(s"parsed primary ${ret}")
    ret
  }

  /* 
   * call -> primary ( "(" + args? + ")" )*
   *
   * Left recursive version:
   *
   * call -> call "(" args? ")"
   *       | primary
   */
  def parseCall: Expression = {
    var call: Expression = parsePrimary
    while (peek.isInstanceOf[Token.LeftParens]) {
      val leftParens = next[Token.LeftParens]
      val args = if (!peek.isInstanceOf[Token.RightParens]) parseArgs else List()
      val rightParens = next[Token.RightParens]
      call = new Expression.Call(call, args, leftParens.l, leftParens.c)
    }

    call
  }

  /*
   * args -> expression ("," args)?
   */
  def parseArgs: List[Expression] = parseArgs()
  def parseArgs(argsSoFar: List[Expression] = List()): List[Expression] = {
    val thisArg = parseExpression
    peek match {
      case _: Token.Comma => {next[Token.Comma]; parseArgs(argsSoFar :+ thisArg)}
      case _ => argsSoFar :+ thisArg
    }
  }

  /*
   * argNames -> identifier ("," args)?
   */
  def parseArgNames: List[Expression.Identifier] = parseArgNames()
  def parseArgNames(argsSoFar: List[Expression.Identifier] = List()): List[Expression.Identifier] = {
    val id = next[Token.Identifier]
    val thisArg = new Expression.Identifier(id.name, id.l, id.c)
    peek match {
      case _: Token.Comma => {next[Token.Comma]; parseArgNames(argsSoFar :+ thisArg)}
      case _ => argsSoFar :+ thisArg
    }
  }

  /*
   * unary -> "!" unary
   *        | call
   */
  def parseUnary: Expression = peek match {
    case Token.Minus(l, c) => {
      next[Token]
      Expression.Minus(parseUnary, l, c)
    }
    case Token.Bang(l, c) => {
      next[Token]
      Expression.Not(parseUnary, l, c)
    }
    case _ => parseCall
  }

  /*
   * multiplication -> unary ("*" | "/") multiplication
   *                 | unary
   */
  def parseMultiplication: Expression = {
    val expr = parseUnary
    
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

  /*
   * addition -> multiplication ("+"|"-") addition
   *           | multiplication
   */
  def parseAddition: Expression = {
    val expr = parseMultiplication

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

  /*
   * comparison -> addition ("<"|">"|"<="|">=") addition
   *             | addition
   */
  def parseComparison: Expression = {
    val expr = parseAddition

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
  
  /*
   * equality -> comparison ("!="|"==") comparison
   *           | comparison
   */
  def parseEquality: Expression = {
    val expr = parseComparison

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

  /* 
   * stmt -> printStmt 
   *       | letStmt
   *       | varStmt
   *       | ifStmt
   *       | exprStmt
   *
   * printStmt -> "print" expression ";"
   * letStmt -> "let" identifier "=" expression ";"
   * varStmt -> "var" identifier "=" expression ";"
   * ifStmt -> "if" expression block ( "else" block )? ";"
   * exprStmt -> expression ";"
   */
  def parseStmt: Stmt = peek match {
    case Token.Print(l, c) => {
      next[Token.Print]
      val expr = parseExpression
      maybeNext[Token.Semicolon] 
      Stmt.Print(expr, l, c)
    }
    case Token.Let(l, c) => {
      next[Token.Let]
      val id = next[Token.Identifier]
      next[Token.Equals]
      val expr = parseExpression
      maybeNext[Token.Semicolon] 
      Stmt.Let(Expression.Identifier(id.name, id.line, id.column), expr, l, c)
    }
    case Token.Var(l, c) => {
      next[Token.Var]
      val id = next[Token.Identifier]
      next[Token.Equals]
      val expr = parseExpression
      maybeNext[Token.Semicolon] 
      Stmt.Var(Expression.Identifier(id.name, id.line, id.column), expr, l, c)
    }
    case Token.If(l, c) => {
      next[Token.If]
      val condition = parseExpression
      val ifBlock = parseBlock

      val elseBlock: Block = if (peek.isInstanceOf[Token.Else]) {
        next[Token.Else]
        parseBlock
      } else List()

      new Stmt.If(condition, ifBlock, elseBlock, l, c)
    }

    case Token(l, c) => {
      val expr = parseExpression
      maybeNext[Token.Semicolon] 
      Stmt.ExprStmt(expr, l, c)
    }
  } 

  /*
   * block -> "{" Stmt* "}"
   *        | Stmt
   */
  def parseBlock: Block = {
    val statements = new ArrayBuffer[Stmt]
    val braced = peek.isInstanceOf[Token.LeftBrace]

    if (braced) {
      next[Token.LeftBrace]
      while (!peek.isInstanceOf[Token.RightBrace]) {
        statements += parseStmt
      }
      next[Token.RightBrace]
    } else statements += parseStmt

    statements.toList
  }

  def checkAs[T](t: Token)(implicit tag: ClassTag[T]) = 
    Try(tag.runtimeClass.cast(t).asInstanceOf[T]) getOrElse {cantParse(t)}

  def next[T <: Token](implicit tag: ClassTag[T]): T = {
    val token = allTokens(this.index)
    this.index += 1

    val callerName: String = Thread.currentThread.getStackTrace()(2).getMethodName
    log(s"--$callerName Consumed $token as ${tag.toString}")

    checkAs[T](token)(tag)
  }

  def maybeNext[T <: Token](implicit tag: ClassTag[T]): Option[T] =
    Try({tag.runtimeClass.cast(peek).asInstanceOf[T]; next[T]}) toOption


  private def peek: Token = allTokens(this.index)

  private def log(str: String) = if (loglevel) println(str) else Unit

  private def cantParse(token: Token) = throw new ParserException(token)
}
