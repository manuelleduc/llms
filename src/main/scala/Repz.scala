import scala.language.{higherKinds, postfixOps}

trait Base {
  type Rep[+T]

  implicit def unit(x: Double): Rep[Double]
}

trait Arith extends Base {


  def num_mult(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  def num_add(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  def num_sub(value: Rep[Double], value1: Rep[Double]): Rep[Double]

  implicit def numbericToNumericMult(x: Double): NumericOpsCls = new NumericOpsCls(unit(x))

  implicit def numbericToNumericRepMult(x: Rep[Double]): NumericOpsCls = new NumericOpsCls(x)

  class NumericOpsCls(lhs: Rep[Double]) {
    def *(rhs: Rep[Double]): Rep[Double] = num_mult(lhs, rhs)

    def +(rhs: Rep[Double]): Rep[Double] = num_add(lhs, rhs)

    def -(rhs: Rep[Double]): Rep[Double] = num_sub(lhs, rhs)
  }

}


trait Trig extends Base {
  def cos(x: Rep[Double]): Rep[Double]

  def sin(x: Rep[Double]): Rep[Double]
}


trait FFT {
  this: Arith with Trig =>

  def omega(k: Int, N: Int): Complex = {
    val kth: Double = -2.0 * k * Math.PI / N
    Complex(cos(kth), sin(kth))
  }


  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

  def splitEvenOdd(complexes: List[FFT.this.Complex]): (List[Complex], List[Complex])

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)

    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)

    def *(that: Complex) = ???
  }

}

trait PowerA {
  this: Arith =>

  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0 else b * power(b, x - 1) // b * power(b, x - 1)
}


/* START BAD*/
trait BaseStr extends Base {
  override type Rep[+T] = String
}

trait ArithStr extends Arith with BaseStr {
  override implicit def unit(x: Double): String = x.toString

  override def num_add(x: String, y: String) = s"($x+$y)"

  override def num_mult(x: String, y: String): String = s"($x*$y)"

  override def num_sub(x: String, y: String): String = s"($x-$y)"
}

trait TrigStr extends Trig with BaseStr {
  override def cos(x: String): String = s"cos($x)"

  override def sin(x: String): String = s"sin($x)"
}

trait PowerB {
  this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0
    else if ((x & 1) == 0) {
      val y = power(b, x / 2)
      y * y
    } else b * power(b, x - 1)
}

//class Repz {
//  val x: PowerA with Arith => (p#Rep[Double]) forSome {type p >: PowerA with Arith <: PowerA with Arith} = (p: PowerA with Arith) => {
//    import p._
//    power(unit(1), 2)
//  }
//}

/*END BAD*/

trait Expressions {

  /* def fresh[T]: Sym[T]

   def findDefinition[T](s: Sym[T]): Option[Def[T]]

   def findDefinition[T](d: Def[T]): Option[Sym[T]]

   def findOrCreateDefinition[T](d: Def[T]): Sym[T]*/


  /* Implicitly convert a composite Def to an atomic Exp
   * Doing so allow to keep a internal cache of already resolved def,
   * hence avoiding generated code duplication.
  */
  implicit def toAtom[T](d: Def[T]): Exp[T] = findOrCreateDefinition(d)

  var nVars = 0
  var globalDefsCache: Map[Sym[Any], Stm] = Map.empty

  def defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(_, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] = globalDefsCache.get(s)

  implicit def intToSym[T](n: Int): Sym[T] = Sym(n)

  def exportGraph(v: Exp[Double]) = ???

  // atomic syntactic elements
  abstract class Exp[+T]

  // composite syntactic elements
  abstract class Def[+T]

  abstract class Stm extends Exp

  // literal values
  case class Const[T](x: T) extends Exp[T]

  // Unique symbol associated to each definition. ANF style (https://en.wikipedia.org/wiki/A-normal_form)
  case class Sym[+T](n: Int) extends Exp[T]

  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match {
      case _ => ???
    }
  }

  def findOrCreateDefinition[T](d: Def[T]): Stm
  = {
    findDefinition[T](d) getOrElse {
      createDefinition(fresh[T], d)
    }
  }


  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    TP(s, d)
  }


  def findDefinition[T](d: Def[T]): Option[Stm] = globalDefsCache
    .find(x => defines(x._2, d).nonEmpty)
    .map(_._2)

  def defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], _) => Some(sym)
    case _ => None
  }

  def fresh[T]: Sym[T] = {
    nVars += 1
    nVars - 1
  }


}

trait BaseExp extends Base with Expressions {
  override type Rep[+T] = Exp[T]
}


trait ArithInterpreter extends Arith with Interpreter {
  override def num_mult(value: Double, value1: Double) = value * value1

  override def num_add(value: Double, value1: Double) = value + value1

  override def num_sub(value: Double, value1: Double) = value - value1
}

trait ArithExp extends Arith with BaseExp {

  def num_mult(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Times(value, value1)

  def num_add(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Plus(value, value1)

  def num_sub(value: Exp[Double], value1: Exp[Double]): Exp[Double] = Sub(value, value1)

  implicit override def unit(x: Double): Exp[Double] = Const(x)

  case class Plus(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  case class Sub(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  case class Times(x: Exp[Double], y: Exp[Double]) extends Def[Double]

  class NumericOpsCls(lhs: Exp[Double]) {
    def *(rhs: Exp[Double]): Exp[Double] = num_mult(lhs, rhs)

    def +(rhs: Exp[Double]): Exp[Double] = num_add(lhs, rhs)

    def -(rhs: Exp[Double]): Exp[Double] = num_sub(lhs, rhs)
  }


  implicit def doubleToExp(x: Double): NumericOpsCls = new NumericOpsCls(unit(x))

  implicit def doubleToExp(x: Exp[Double]): NumericOpsCls = new NumericOpsCls(x)


}


trait TrigExp extends Trig with BaseExp {

  override def sin(x: Exp[Double]): Exp[Double] = Sin(x)

  override def cos(x: Exp[Double]): Exp[Double] = Cos(x)

  case class Sin(x: Exp[Double]) extends Def[Double]

  case class Cos(x: Exp[Double]) extends Def[Double]

}


trait Compile extends Base {
  def compile[A, B](f: Rep[A] => Rep[B]): A => B
}


trait Interpreter extends Base {
  override type Rep[+T] = T

  override def unit(x: Double): Double = x
}

trait ScalaGenBase extends BaseExp {
  def buildSchedule(e: Exp[_]): List[(Sym[_], Def[_])] = List()

  def emitNode(sym: Sym[_], node: Def[_]): Unit = throw new Exception(s"node $node not supported")
}


trait ScalaGenArith extends ScalaGenBase with ArithExp {
  override def emitNode(sym: Sym[_], node: Def[_]) = node match {
    case Plus(a, b) => println(s"val $sym = $a + $b")
    case Times(a, b) => println(s"val $sym = $a * $b")
    case _ => super.emitNode(sym, node)
  }
}

trait CompileScala extends Compile with ScalaGenBase {
  override def compile[A, B](f: Exp[A] => Exp[B]): A => B = {
    val x = fresh[A]
    val y = f(x)

    val l: List[(Sym[_], Def[_])] = buildSchedule(y)
    for ((sym, node) <- l) {
      emitNode(sym, node)
    }

    // emit footer
    // invoke compiler
    // load generate class file
    // instantiate object of that class
    ???
  }
}


trait PowerA2 {
  this: Compile with Arith with PowerA =>
  val p4 = compile { x: Rep[Double] =>
    power(x + x, 4)
  }

  println(p4(6))
}

trait PowerA2b {
  this: Compile with Arith with PowerA =>

  trait Tmp extends Arith with PowerA {
    def calc (x:Double) {
      power(x + x, 4)
    }
  }

  trait CompileScalaTmp extends CompileScala with Tmp

  val p2 = new CompileScalaTmp {} // TODO: comment avoir compilation et interpretation sur le même programme???

  trait WrapCompiler {
    this: Compiler =>
    def compile()
  }

  println(p4(6))
}

trait PowerA3 {
  this: Interpreter with Arith with PowerA =>
  val p4 = { x: Rep[Double] =>
    power(x + x, 4)
  }

  println(p4(6))
}


object TestPower extends App {
  //new PowerA2 with ScalaGenArith with CompileScala with ArithExp with PowerA
  new PowerA3 with PowerA with ArithInterpreter
}
