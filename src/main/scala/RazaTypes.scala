package Raza

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

  def __add__(other: RazaObject): RazaObject = 
    if (this.isInstanceOf[RazaString] || other.isInstanceOf[RazaString]) 
      new RazaString(this.__str__.value + other.__str__.value)
    else
      binaryExceptionMessage("add", other)
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
  def __call__(args: List[RazaObject]): RazaObject = unaryExceptionMessage("call")
}

case class RazaString(val value: String) extends RazaObject {
  override def __add__(other: RazaObject): RazaString = 
    new RazaString(this.value + other.__str__.value)

  override def __eq__(other: RazaObject): RazaObject = other match {
    case RazaString(str) => new RazaBool(this.value == str)
    case _ => super.__eq__(other)
  }

  override def __neq__(other: RazaObject): RazaObject = other match {
    case RazaString(str) => new RazaBool(this.value != str)
    case _ => super.__neq__(other)
  }

  override def __str__(): RazaString = this
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

  override def __str__() = new RazaString("" + this.value)
}

case class RazaBool(val value: Boolean) extends RazaObject {
  override def __not__(): RazaBool = new RazaBool(!value)

  // Just for readability
  def unary_!(): RazaBool = this.__not__
}

case class RazaFunction(val argNames: List[String], val body: Block, val closure: Environment)
extends RazaObject {
  override def __call__(args: List[RazaObject]) = {
    val env = new Environment(closure) ++ (argNames zip args.map(v => Constant(v)))
    Interpreter.execBlock(body, env)
  }
}

case class RazaNil() extends RazaObject 


