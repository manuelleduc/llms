package llms.arith

import llms.power.{Power, PowerA, PowerB}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class ArithTest extends Properties("ArithTest") {

  trait PowerA2 extends Arith {
    this: Power =>
    def f(x: Rep[Double]): Rep[Double] = power(x, 10)
  }

  val a = new PowerA2 with ArithInterpreter with PowerA
  val b = new PowerA2 with ArithInterpreter with PowerB


  property("Implementations iso-functionals A/default") =
    forAll {
      (x: Double) => {
        val impleA: Double = a.f(x)
        val default = Math.pow(x, 10)
        impleA == default
      }
    }

  property("Implementations iso-functionals B/default") =
    forAll {
      (x: Double) => {
        val implemB = b.f(x)
        val default = Math.pow(x, 10)
        implemB == default
      }
    }

  property("Implementations iso-functionals A/B") =
    forAll {
      (x:Double) => {
        val impleA: Double = a.f(x)
        val implemB = b.f(x)
        impleA == implemB
      }
    }
}
