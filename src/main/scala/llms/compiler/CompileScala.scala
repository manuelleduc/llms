package llms.compiler

import llms.base.ScalaGenBase

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
