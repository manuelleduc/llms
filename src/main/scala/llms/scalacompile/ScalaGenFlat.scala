package llms.scalacompile

import llms.gen.ScalaGenBase

trait ScalaGenFlat extends ScalaGenBase {

  import IR._

  override type Block[+T] = Exp[T]

  override def getBlockResultFull[T](x: Block[T]): Exp[T] = x

  override def reifyBlock[T](x: => Exp[T]): Block[T] = x

  override def traverseBlock[A](block: Block[A]): Unit = {
    buildScheduleForResult(block) foreach traverseStm
  }

}
