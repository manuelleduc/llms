package llms.base

import llms.expressions.Expressions

trait BaseExp extends Base with Expressions {
  override type Rep[+T] = Exp[T]
}
