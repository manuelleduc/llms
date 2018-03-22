package llms.power

import llms.arith.Arith

trait Power {
  this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double]
}
