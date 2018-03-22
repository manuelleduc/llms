package llms.gen

import llms.expressions.Expressions

trait ScalaGenBase extends ScalaCodegen {

  val IR: Expressions

  import IR._

  def buildSchedule(e: Exp[_]): List[(Sym[_], Def[_])] = List()

  /**
    * Emit node is specialized for dormain specific nodes.
    *
    * @param sym  a symbol
    * @param node a node
    */
  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = throw new Exception(s"node $node not supported")

}
