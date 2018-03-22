package llms.lineralgebra

trait LineratorAlgebraOpt extends LinearAlgebraExp {
  override def vector_scale(v: Exp[Seq[Double]], k: Exp[Double]): Exp[Seq[Double]] = k match {
    case Const(1.0) => v
    case _ => super.vector_scale(v, k)
  }
}
