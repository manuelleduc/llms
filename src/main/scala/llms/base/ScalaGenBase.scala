package llms.base

trait ScalaGenBase extends BaseExp {
  def buildSchedule(e: Exp[_]): List[(Sym[_], Def[_])] = List()

  def emitNode(sym: Sym[_], node: Def[_]): Unit = throw new Exception(s"node $node not supported")
}
