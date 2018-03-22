package llms.gen

import java.io.PrintWriter

import llms.expressions.Expressions

trait GenericCodegen extends BlockTraversal {
  val IR: Expressions

  import IR._

  def emitSource[A](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter): List[(Sym[Any], Any)] // return free static data in block

  var stream: PrintWriter = _

  def emitFileHeader(): Unit = {}

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) => emitNode(sym, rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try {
      body
    } finally {
      stream.flush()
      stream = save
    }
  }

  def emitSource[T , R ](f: Exp[T] => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s = fresh[T]
    val body = reifyBlock(f(s))
    emitSource(List(s), body, className, stream)
  }

  def remap[A](m: A): String = m.toString // TODO: probably wrong
}
