package llms.arith

import llms.gen.ScalaGenBase

trait ScalaGenArith extends ScalaGenBase {
  val IR: ArithExp

  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case Plus(a, b) => println(s"val $sym = $a + $b")
    case Times(a, b) => println(s"val $sym = $a * $b")
    case _ => super.emitNode(sym, node)
  }
}
