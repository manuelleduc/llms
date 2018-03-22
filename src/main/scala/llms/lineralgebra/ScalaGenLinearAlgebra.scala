package llms.lineralgebra

import llms.gen.ScalaGenBase

trait ScalaGenLinearAlgebra extends ScalaGenBase {
  val IR: LinearAlgebraExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case VectorScale(v, k) =>
      emitValDef(sym, s"${quote(v)}.map(x => x * ${quote(k)})")
    case _ => super.emitNode(sym, node)
  }
}
