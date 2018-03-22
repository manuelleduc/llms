package llms.trig

import llms.base.BaseExp

trait TrigExp extends Trig with BaseExp {

  override def sin(x: Exp[Double]): Exp[Double] = Sin(x)

  override def cos(x: Exp[Double]): Exp[Double] = Cos(x)

  case class Sin(x: Exp[Double]) extends Def[Double]

  case class Cos(x: Exp[Double]) extends Def[Double]

}
