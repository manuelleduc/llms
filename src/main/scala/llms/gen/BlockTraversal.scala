package llms.gen

import llms.expressions.Expressions

import scala.language.higherKinds

trait BlockTraversal extends GraphTraversal {
  val IR: Expressions

  import IR._

  type Block[+T]


  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any], Any)] = Nil // TODO: Nil or Exception??

  def traverseBlock[A](block: Block[A]): Unit

  def traverseStm(stm: Stm): Unit

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res

  def getBlockResultFull[A](s: Block[A]): Exp[A]

  def reifyBlock[T](x: => Exp[T]): Block[T]
}
