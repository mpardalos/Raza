package Raza

import collection.immutable.HashMap
import util.Try

sealed abstract class Value(val value: RazaObject)
case class Constant(v: RazaObject) extends Value(v)
case class Variable(v: RazaObject) extends Value(v)

sealed abstract class AssignmentFailure
case object IsConstant extends AssignmentFailure
case object AssignedVarToConst extends AssignmentFailure
case object AssignedConstToVar extends AssignmentFailure

class Environment(_parent: Option[Environment], _contents: Map[String, Value]) {
  private val parent: Option[Environment] = _parent
  private val contents: Map[String, Value] = _contents

  def this() = this(None, new HashMap())
  def this(_parent: Environment) = this(Some(_parent), new HashMap())
  def this(_parent: Option[Environment]) = this(_parent, new HashMap())

  def get(key: String): Option[Value] = contents.get(key) match {
    case s @ Some(_) => s
    case None => parent.flatMap(_.get(key))
  }

  def ++(other: Map[String, Value]) = new Environment(parent, contents ++ other)
  def ++(other: Environment) = new Environment(parent, contents ++ other.contents)

  /*
   * Add a String-Value pair to the environment if it is allowed by the value types
   * (Constant/Variable)
   */
  def +(kv: (String, Value)): Either[AssignmentFailure, Environment] = kv match {
    case (name, v: Variable) => get(name) match {
      case Some(Variable(_)) | None => Right(new Environment(parent, contents + kv))
      case Some(Constant(_)) => Left(AssignedVarToConst)
    }
    case (name, v: Constant) => get(name) match {
      case None => Right(new Environment(parent, contents + kv))
      case Some(Constant(_)) => Left(IsConstant)
      case Some(Variable(_)) => Left(AssignedConstToVar)
    }
  }
}

object Interpreter {
  import Stmt._
  import Expression._

  def execBlock(block: Block, env: Environment = new Environment()): RazaObject = 
    block match {
      case ExprStmt(expr, _, _) :: Nil => evaluate(expr, env)
      case stmt :: stmts => execBlock(stmts, exec(stmt, env))
      case Nil => new RazaNil()
    }

  /*
   * @param stmt The statement to execute
   * @param env The environment to execute it in 
   * @return The modified environment after stmt has been executed
   */
  def exec(stmt: Stmt, env: Environment): Environment = try {
    stmt match {
      case Print(expr, _, _) => println(evaluate(expr, env).printableString); env

      case ExprStmt(expr, _, _) => evaluate(expr, env); env

      case d @ Declaration(Identifier(name, _, _), expr, l, c) => (d match {
        case _: Var => env + (name -> Variable(evaluate(expr, env)))
        case _: Let => env + (name -> Constant(evaluate(expr, env)))
      }) match {
        case Right(env) => env
        case Left(AssignedVarToConst | IsConstant) => 
          runtimeException(l, c, s"Cannot assign to ${name}. It is a constant")
        case Left(AssignedConstToVar) =>
          runtimeException(l, c, s"Cannot assign to variable ${name} using let. Use var instead")
      }

    }
  } catch {
    case PartialRazaRuntimeException(msg) =>
      runtimeException(stmt.line, stmt.column, msg)
  }

  /* 
   * @param expr the expression to evaluate
   * @param env the environment to evaluate it in
   * @return The value of the expression
   */
  def evaluate(expr: Expression, env: Environment): RazaObject = expr match {
    case Identifier(name, l, c) => env get name match {
      case Some(value) => value.value
      case None => runtimeException(l, c, s"Identifier $name not defined")
    }
    case Str(str, _, _) => new RazaString(str)
    case Integer(num, _, _) => new RazaNumber(num)
    case Decimal(num, _, _) => new RazaNumber(num)
    case True(_, _) => new RazaBool(true)
    case False(_, _) => new RazaBool(false)
    case Nil_(_, _) => new RazaNil
    case FunctionDef(args, body, _, _) => new RazaFunction(args.map(_.name), body, env)

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

    case Call(callee, args, _, _) => evaluate(callee, env)
      .__call__(args.map {evaluate(_, env)}, env)

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
