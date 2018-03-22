package llms.gen

import llms.expressions.Expressions

trait GraphTraversal extends Scheduling {
  val IR: Expressions
  import IR._

  def availableDefs: List[Stm] = globalDefs

  def buildScheduleForResult(result: Any, sort: Boolean = true): List[Stm] =
    getSchedule(availableDefs)(result, sort)

  def getDependentStuff(st: List[Sym[Any]]): List[Stm] = {
    getFatDependentStuff(availableDefs)(st)
  }

  def getDependentStuff(st: Sym[Any]): List[Stm] = {
    getDependentStuff(List(st))
  }
}
