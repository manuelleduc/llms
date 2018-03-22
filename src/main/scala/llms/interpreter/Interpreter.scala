package llms.interpreter

import llms.base.Base

trait Interpreter extends Base {
  override type Rep[+T] = T

  override def unit(x: Double): Double = x
}
