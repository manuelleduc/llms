package llms.arith

import llms.base.ScalaGenBase

trait ScalaGenArith extends ScalaGenBase with ArithExp {
  override def emitNode(sym: Sym[_], node: Def[_]): Unit = node match {
    case Plus(a, b) => println(s"val $sym = $a + $b")
    case Times(a, b) => println(s"val $sym = $a * $b")
    case _ => super.emitNode(sym, node)
  }
}
