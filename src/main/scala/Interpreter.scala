package Raza

import collection.mutable._

case class RazaRuntimeException(line: Int, column: Int, msg: String) extends Exception
case class PartialRazaRuntimeException(msg: String) extends Exception

abstract class RazaObject {
  private def binaryExceptionMessage(operation: String, other: RazaObject) = 
    throw new PartialRazaRuntimeException(
      s"Can't $operation ${this.getClass.getSimpleName} and ${other.getClass.getSimpleName}"
    )

  private def unaryExceptionMessage(operation: String) =
    throw new PartialRazaRuntimeException(
      s"Can't $operation ${this.getClass.getSimpleName}"
    )

  def printableString: String = this.__str__.value

  def __add__(other: RazaObject): RazaObject = binaryExceptionMessage("add", other)
  def __minus__(other: RazaObject): RazaObject = binaryExceptionMessage("subtract", other)
  def __mul__(other: RazaObject): RazaObject = binaryExceptionMessage("multiply", other)
  def __div__(other: RazaObject): RazaObject = binaryExceptionMessage("divide", other)
  def __eq__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __neq__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __le__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __gr__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __leq__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __geq__(other: RazaObject): RazaObject = binaryExceptionMessage("compare", other)
  def __neg__(): RazaObject = unaryExceptionMessage("invert")
  def __not__(): RazaObject = unaryExceptionMessage("negate")
  def __str__(): RazaString = unaryExceptionMessage("stringify")
}

case class RazaString(val value: String) extends RazaObject {
  override def __add__(other: RazaObject) = other match {
    case RazaString(str) => new RazaString(this.value + str)
    case _ => super.__add__(other)
  }

  override def __eq__(other: RazaObject): RazaObject = other match {
    case RazaString(str) => new RazaBool(this.value == str)
    case _ => super.__eq__(other)
  }

  override def __neq__(other: RazaObject): RazaObject = other match {
    case RazaString(str) => new RazaBool(this.value != str)
    case _ => super.__neq__(other)
  }

  override def __str__() = this
}

class DoubleOrInt[T]
object DoubleOrInt {
  implicit object IntWitness extends DoubleOrInt[Int]
  implicit object DoubleWitness extends DoubleOrInt[Double]
}

case class RazaNumber[T: DoubleOrInt](val value: T) extends RazaObject {
  private def numCombine(intCase: (Int, Int) => Int,
                         doubleCase: (Double, Double) => Double,
                         default: RazaObject => RazaObject)
                        (other: RazaObject): RazaObject = other match {
    case RazaNumber(otherNum: Int) => value match {
      case num: Int => new RazaNumber(intCase(num, otherNum))
      case num: Double => new RazaNumber(doubleCase(num, otherNum.toDouble))
    }
    case RazaNumber(otherNum: Double) => value match {
      case num: Int => new RazaNumber(doubleCase(num.toDouble, otherNum))
      case num: Double => new RazaNumber(doubleCase(num, otherNum))
    }
    case _ => default(other)
  }

  private def boolCombine(intCase: (Int, Int) => Boolean,
                          doubleCase: (Double, Double) => Boolean,
                          default: RazaObject => RazaObject)
                         (other: RazaObject): RazaObject = other match {
    case RazaNumber(otherNum: Int) => value match {
      case num: Int => new RazaBool(intCase(num, otherNum))
      case num: Double => new RazaBool(doubleCase(num, otherNum.toDouble))
    }
    case RazaNumber(otherNum: Double) => value match {
      case num: Int => new RazaBool(doubleCase(num.toDouble, otherNum))
      case num: Double => new RazaBool(doubleCase(num, otherNum))
    }
    case _ => default(other)
  }


  override def __add__(other: RazaObject) = numCombine(_+_, _+_, super.__add__)(other)
  override def __minus__(other: RazaObject) = numCombine(_-_, _-_, super.__minus__)(other)
  override def __div__(other: RazaObject) = numCombine(_/_, _/_, super.__div__)(other)
  override def __mul__(other: RazaObject) = numCombine(_*_, _*_, super.__mul__)(other)

  override def __eq__(other: RazaObject) = boolCombine(_==_, _==_, super.__eq__)(other)
  override def __neq__(other: RazaObject) = boolCombine(_!=_, _!=_, super.__neq__)(other)
  override def __gr__(other: RazaObject) = boolCombine(_>_, _>_, super.__gr__)(other)
  override def __le__(other: RazaObject) = boolCombine(_<_, _<_, super.__le__)(other)
  override def __leq__(other: RazaObject) = boolCombine(_<=_, _<=_, super.__leq__)(other)
  override def __geq__(other: RazaObject) = boolCombine(_>=_, _>=_, super.__geq__)(other)

  override def __str__() = value match {
    case v: Int => new RazaString(v.toString)
    case v: Double => new RazaString(v.toString)
  }

}

case class RazaBool(val value: Boolean) extends RazaObject {
  override def __not__(): RazaBool = new RazaBool(!value)

  /*
   * Just for readability
   */
  def unary_!(): RazaBool = this.__not__
}

case class RazaNil() extends RazaObject 

class Interpreter(val ast: List[Stmt]) {
  import Stmt._
  import Expression._

  val baseEnv: Map[String, RazaObject] = new HashMap()

  def interpretAll = ast.foreach {stmt => exec(stmt)}

  def exec(stmt: Stmt): Unit = try {
    stmt match {
      case Print(expr, _, _) => println(evaluate(expr).printableString)
      case Let(identifier, expr, l, c) => if (baseEnv contains identifier.name) 
        throw new RazaRuntimeException(l, c, s"Cannot assign to ${identifier.name}. Value is immutable")
        else baseEnv += (identifier.name -> evaluate(expr))
      case ExprStmt(expr, _, _) => evaluate(expr)
    }
  } catch {
    case PartialRazaRuntimeException(msg) =>
      throw new RazaRuntimeException(stmt.line, stmt.column, msg)
  }

  def evaluate(expr: Expression): RazaObject = expr match {
    case Identifier(name, l, c) => baseEnv getOrElse(name, 
      {throw new RazaRuntimeException(l, c, s"Identifier $name not defined")})

    case Str(str, _, _) => new RazaString(str)
    case Integer(num, _, _) => new RazaNumber(num)
    case Decimal(num, _, _) => new RazaNumber(num)
    case True(_, _) => new RazaBool(true)
    case False(_, _) => new RazaBool(false)
    case Nil(_, _) => new RazaNil
    case Addition(left, right, _, _) => evaluate(left).__add__(evaluate(right))
    case Subtraction(left, right, _, _) => evaluate(left).__minus__(evaluate(right))
    case Multiplication(left, right, _, _) => evaluate(left).__mul__(evaluate(right))
    case Division(left, right, _, _) => evaluate(left).__div__(evaluate(right))
    case Equal(left, right, _, _) => evaluate(left).__eq__(evaluate(right))
    case NotEqual(left, right, _, _) => evaluate(left).__neq__(evaluate(right))
    case Less(left, right, _, _) => evaluate(left).__le__(evaluate(right))
    case Greater(left, right, _, _) => evaluate(left).__gr__(evaluate(right))
    case LessEqual(left, right, _, _) => evaluate(left).__leq__(evaluate(right))
    case GreaterEqual(left, right, _, _) => evaluate(left).__geq__(evaluate(right))
    case Not(expr, _, _) => evaluate(expr).__not__
    case Minus(expr, _, _) => evaluate(expr).__neg__
  }
  } 
