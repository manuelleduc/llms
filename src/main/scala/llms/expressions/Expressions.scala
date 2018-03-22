package llms.expressions

import scala.language.implicitConversions

trait Expressions {

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

  def findOrCreateDefinition[T](d: Def[T]): Stm = findDefinition[T](d) getOrElse {
    createDefinition(fresh[T], d)
  }

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = TP(s, d)

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
