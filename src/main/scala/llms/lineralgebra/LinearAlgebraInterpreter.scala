package llms.lineralgebra

import llms.interpreter.Interpreter

trait LinearAlgebraInterpreter extends LinearAlgebra with Interpreter {
  override type Vector = Seq[Double]

  override def vector_scale(v: Seq[Double], k: Double): Seq[Double] = v map (_ * k)
}
