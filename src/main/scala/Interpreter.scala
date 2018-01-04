package Raza

case class RazaRuntimeException(line: Int, column: Int, msg: String) extends Exception
case class PartialRazaRuntimeException(msg: String) extends Exception


abstract class RazaObject {
  private def binaryExceptionMessage(operation: String, other: RazaObject) = 
    s"Can't $operation ${this.getClass.getSimpleName} and ${other.getClass.getSimpleName}"

  private def unaryExceptionMessage(operation: String) =
    s"Can't $operation ${this.getClass.getSimpleName}"

  def __add__(other: RazaObject): RazaObject = 
    throw new PartialRazaRuntimeException(binaryExceptionMessage("add", other))

  def __minus__(other: RazaObject): RazaObject = 
    throw new PartialRazaRuntimeException(binaryExceptionMessage("subtract", other))

  def __mul__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("multiply", other))

  def __div__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("divide", other))

  def __eq__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __neq__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __le__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __gr__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __leq__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __geq__(other: RazaObject): RazaObject =
    throw new PartialRazaRuntimeException(binaryExceptionMessage("compare", other))

  def __neg__(): RazaObject = 
    throw new PartialRazaRuntimeException(unaryExceptionMessage("invert"))

  def __not__(): RazaObject =
    throw new PartialRazaRuntimeException(unaryExceptionMessage("negate"))
}

case class RazaString(val value: String) extends RazaObject {
  override def toString = "\"" + this.value + "\""

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


  override def __add__(other: RazaObject): RazaObject = numCombine(_+_, _+_, super.__add__)(other)
  override def __minus__(other: RazaObject): RazaObject = numCombine(_-_, _-_, super.__minus__)(other)
  override def __div__(other: RazaObject): RazaObject = numCombine(_/_, _/_, super.__div__)(other)
  override def __mul__(other: RazaObject): RazaObject = numCombine(_*_, _*_, super.__mul__)(other)

  override def __eq__(other: RazaObject): RazaObject = boolCombine(_==_, _==_, super.__eq__)(other)
  override def __neq__(other: RazaObject): RazaObject = boolCombine(_!=_, _!=_, super.__neq__)(other)

  override def __gr__(other: RazaObject): RazaObject = boolCombine(_>_, _>_, super.__gr__)(other)
  override def __le__(other: RazaObject): RazaObject = boolCombine(_<_, _<_, super.__le__)(other)

  override def __leq__(other: RazaObject): RazaObject = boolCombine(_<=_, _<=_, super.__leq__)(other)
  override def __geq__(other: RazaObject): RazaObject = boolCombine(_>=_, _>=_, super.__geq__)(other)

}

case class RazaBool(val value: Boolean) extends RazaObject {
  override def __not__(): RazaBool = new RazaBool(!value)

  /*
   * Just for readability
   */
  def unary_!(): RazaBool = this.__not__
}

case class RazaNil() extends RazaObject 

class Interpreter(val ast: AST) {
  def interpretAll = evaluate(ast)

  def evaluate(expr: Expression): RazaObject = try {
    expr match {
      case Expression.Str(str, _, _) => new RazaString(str)
      case Expression.Integer(num, _, _) => new RazaNumber(num)
      case Expression.Decimal(num, _, _) => new RazaNumber(num)
      case Expression.True(_, _) => new RazaBool(true)
      case Expression.False(_, _) => new RazaBool(false)
      case Expression.Nil(_, _) => new RazaNil
      case Expression.Addition(left, right, _, _) => evaluate(left).__add__(evaluate(right))
      case Expression.Subtraction(left, right, _, _) => evaluate(left).__minus__(evaluate(right))
      case Expression.Multiplication(left, right, _, _) => evaluate(left).__mul__(evaluate(right))
      case Expression.Division(left, right, _, _) => evaluate(left).__div__(evaluate(right))
      case Expression.Equal(left, right, _, _) => evaluate(left).__eq__(evaluate(right))
      case Expression.NotEqual(left, right, _, _) => evaluate(left).__neq__(evaluate(right))
      case Expression.Less(left, right, _, _) => evaluate(left).__le__(evaluate(right))
      case Expression.Greater(left, right, _, _) => evaluate(left).__gr__(evaluate(right))
      case Expression.LessEqual(left, right, _, _) => evaluate(left).__leq__(evaluate(right))
      case Expression.GreaterEqual(left, right, _, _) => evaluate(left).__geq__(evaluate(right))
      case Expression.Not(expr, _, _) => evaluate(expr).__not__
      case Expression.Minus(expr, _, _) => evaluate(expr).__neg__
    }
  } catch {
    case PartialRazaRuntimeException(msg) =>
      throw new RazaRuntimeException(expr.line, expr.column, msg)
  }
}
