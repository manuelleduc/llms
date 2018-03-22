package llms.power

import llms.arith.Arith

trait PowerB extends Power {
  this: Arith =>
  override def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0
    else if ((x & 1) == 0) {
      // x is even
      val y = power(b, x / 2)
      y * y
    } else b * power(b, x - 1)
}
