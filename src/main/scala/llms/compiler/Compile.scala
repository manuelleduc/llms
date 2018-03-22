package llms.compiler

import llms.base.Base

trait Compile extends Base {
  def compile[A, B](f: Rep[A] => Rep[B]): A => B
}
