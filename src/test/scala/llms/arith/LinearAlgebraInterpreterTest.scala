package llms.arith

import llms.lineralgebra.{LinearAlgebra, LinearAlgebraInterpreter}
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class LinearAlgebraInterpreterTest extends Properties("LinerarAlgebraInterpreter") {


  val s = 10.0

  trait LinearAlgebraS extends LinearAlgebra {
    this: LinearAlgebra =>
    def f(x: Rep[Vector]) = vector_scale(x, s)
  }

  val la = new LinearAlgebraS with LinearAlgebraInterpreter
  property("Implementation linear algebra") = forAll {
    (x: Seq[Double]) => {
      la.f(x) == (x map (_ * s))
    }
  }

}
