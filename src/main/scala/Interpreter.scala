package Raza

import collection.mutable.HashMap
import util.Try

sealed abstract class Value(val value: RazaObject)
case class Constant(v: RazaObject) extends Value(v)
case class Variable(v: RazaObject) extends Value(v)

sealed abstract class AssignmentFailure
case object IsConstant extends AssignmentFailure
case object AssignedVarToConst extends AssignmentFailure
case object AssignedConstToVar extends AssignmentFailure

class Environment(_parent: Option[Environment]) {
  private val parent: Option[Environment] = _parent
  private val contents: HashMap[String, Value] = new HashMap()

  def this() = this(None)
  def this(_parent: Environment) = this(Some(_parent))

  def get(key: String): Option[Value] = contents.get(key) match {
    case s @ Some(_) => s
    case None => parent.flatMap(_.get(key))
  }
   
  def put(key: String, value: Value): Either[AssignmentFailure, Unit] = value match {
    case v: Variable => get(key) match {
      case Some(Variable(_)) | None => contents put (key, v); Right(Unit)
      case Some(Constant(_)) => Left(AssignedVarToConst)
    }
    case v: Constant => get(key) match {
      case None => contents put (key, v); Right(Unit)
      case Some(Constant(_)) => Left(IsConstant)
      case Some(Variable(_)) => Left(AssignedConstToVar)
    }
  }
}

object Interpreter {
  import Stmt._
  import Expression._

  private val baseEnv: Environment = new Environment()

  def interpretAll(program: Block) = program.stmts.foreach {stmt => exec(stmt, baseEnv)}

  def exec(stmt: Stmt, env: Environment): Unit = try {
    stmt match {
      case Print(expr, _, _) => println(evaluate(expr, env).printableString)

      case ExprStmt(expr, _, _) => evaluate(expr, env)

      case d @ Declaration(identifier, expr, l, c) => 
        env put (identifier.name, d match {
          case _: Var => Variable(evaluate(expr, env))
          case _: Let => Constant(evaluate(expr, env))
        }) match {
          case Right(_) => ()
          case Left(AssignedVarToConst | IsConstant) => 
            runtimeException(l, c, s"Cannot assign to ${identifier.name}. It is a constant")
          case Left(AssignedConstToVar) =>
            runtimeException(l, c, s"Cannot assign to variable ${identifier.name} using let. Use var instead")
        }

      case If(condition, ifBlock, elseBlock, _, _) => {
        val conditionValue = evaluate(condition, env)
        val conditionResult = Try(conditionValue.asInstanceOf[RazaBool]) 
          .getOrElse {runtimeException(condition.line, condition.column, 
            s"If condition must be a RazaBool, not ${conditionValue.getClass.getSimpleName}")}
        
        conditionResult match {
          case RazaBool(true) => execBlock(ifBlock, env)
          case RazaBool(false) => execBlock(elseBlock, env)
        }
      }
    }
  } catch {
    case PartialRazaRuntimeException(msg) =>
      runtimeException(stmt.line, stmt.column, msg)
  }

  def execBlock(block: Block, env: Environment) = block.stmts.foreach {exec(_, env)}

  def evaluate(expr: Expression, env: Environment): RazaObject = expr match {
    case Identifier(name, l, c) => baseEnv get name match {
      case Some(value) => value.value
      case None => runtimeException(l, c, s"Identifier $name not defined")
    }
    case Str(str, _, _) => new RazaString(str)
    case Integer(num, _, _) => new RazaNumber(num)
    case Decimal(num, _, _) => new RazaNumber(num)
    case True(_, _) => new RazaBool(true)
    case False(_, _) => new RazaBool(false)
    case Nil(_, _) => new RazaNil

    case Call(callee, args, _, _) => evaluate(callee, env).__call__(args.map {evaluate(_, env)})
    case FunctionDef(_, _, _, _) => ???

    case Addition(left, right, _, _) => evaluate(left, env).__add__(evaluate(right, env))
    case Subtraction(left, right, _, _) => evaluate(left, env).__minus__(evaluate(right, env))
    case Multiplication(left, right, _, _) => evaluate(left, env).__mul__(evaluate(right, env))
    case Division(left, right, _, _) => evaluate(left, env).__div__(evaluate(right, env))
    case Equal(left, right, _, _) => evaluate(left, env).__eq__(evaluate(right, env))
    case NotEqual(left, right, _, _) => evaluate(left, env).__neq__(evaluate(right, env))
    case Less(left, right, _, _) => evaluate(left, env).__le__(evaluate(right, env))
    case Greater(left, right, _, _) => evaluate(left, env).__gr__(evaluate(right, env))
    case LessEqual(left, right, _, _) => evaluate(left, env).__leq__(evaluate(right, env))
    case GreaterEqual(left, right, _, _) => evaluate(left, env).__geq__(evaluate(right, env))
    case Not(expr, _, _) => evaluate(expr, env).__not__
    case Minus(expr, _, _) => evaluate(expr, env).__neg__
  }

  private def runtimeException(line: Int, column: Int, msg: String) =
    throw new RazaRuntimeException(line, column, msg)
} 
