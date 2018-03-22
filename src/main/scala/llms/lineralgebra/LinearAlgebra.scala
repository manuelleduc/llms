package llms.lineralgebra

import llms.base.Base

trait LinearAlgebra extends Base {

  type Vector

  def vector_scale(v: Rep[Vector], k: Rep[Double]): Rep[Vector]

  implicit class VectorOps(v: Rep[Vector]) {
    def *(k: Rep[Double]): Rep[Vector] = vector_scale(v, k)
  }

}
