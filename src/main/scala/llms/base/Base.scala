package llms.base

import scala.language.higherKinds

trait Base {
  type Rep[+T]

  implicit def unit(x: Double): Rep[Double]
}
