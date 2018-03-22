package llms.expressions

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

trait Expressions {

  /* Implicitly convert a composite Def to an atomic Exp
   * Doing so allow to keep a internal cache of already resolved def,
   * hence avoiding generated code duplication.
  */
  implicit def toAtom[T](d: Def[T]): Exp[T] = findOrCreateDefinitionExp(d)

  def findOrCreateDefinitionExp[T](d: Def[T]): Exp[T] =
    defines(findOrCreateDefinition(d), d).get

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms)
    // All case classes extend Product!
    case p: Product =>
      // performance hotspot: this is the same as
      // p.productIterator.toList.flatMap(syms(_))
      // but faster
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }

  var nVars = 0
  var globalDefs: List[Stm] = Nil
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

  abstract class Stm

  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => sym :: Nil
  }

  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

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

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms)
    case p: Product => p.productIterator.toList.flatMap(boundSyms)
    case _ => Nil
  }

  def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(tunnelSyms)
    case p: Product => p.productIterator.toList.flatMap(tunnelSyms)
    case _ => Nil
  }
}
