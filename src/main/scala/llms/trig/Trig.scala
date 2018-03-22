package llms.trig

import llms.base.Base

trait Trig extends Base {
  def cos(x: Rep[Double]): Rep[Double]

  def sin(x: Rep[Double]): Rep[Double]
}
