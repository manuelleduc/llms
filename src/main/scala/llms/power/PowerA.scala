package llms.power

import llms.arith.Arith

trait PowerA extends Power {
  this: Arith =>
  override def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0 else b * power(b, x - 1)
}
