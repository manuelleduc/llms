package llms.lineralgebra

import llms.base.BaseExp

trait LinearAlgebraExp extends LinearAlgebra with BaseExp {

  case class VectorScale(v: Exp[Vector], k: Exp[Double]) extends Def[Vector]

  override def vector_scale(v: Exp[Vector], k: Exp[Double]): Exp[Vector] = VectorScale(v, k)

  override type Vector = Seq[Double]

}
