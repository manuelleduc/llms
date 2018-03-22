package llms.util

import java.util

object GraphUtil {

  class Ref[T](init: T) {
    var value: T = init
  }

  def stronglyConnectedComponents[T](start: List[T], succ: T => List[T]): List[List[T]] = {

    val id: Ref[Int] = new Ref(0)
    val stack = new util.ArrayDeque[T]
    val mark = new util.HashMap[T, Int]

    val res = new Ref[List[List[T]]](Nil)
    for (node <- start)
      visit(node, succ, id, stack, mark, res)

    res.value
  }

  def visit[T](node: T, succ: T => List[T], id: Ref[Int], stack: util.ArrayDeque[T],
               mark: util.HashMap[T, Int], res: Ref[List[List[T]]]): Int = {


    if (mark.containsKey(node))
      mark.get(node)
    else {
      id.value = id.value + 1

      mark.put(node, id.value)
      stack.addFirst(node)
      //    println("push " + node)

      var min: Int = id.value
      for (child <- succ(node)) {
        val m = visit(child, succ, id, stack, mark, res)

        if (m < min)
          min = m
      }

      if (min == mark.get(node)) {
        var scc: List[T] = Nil
        var loop: Boolean = true
        do {
          val element = stack.removeFirst()
          //        println("appending " + element)
          scc ::= element
          mark.put(element, Integer.MAX_VALUE)
          loop = element != node
        } while (loop)
        res.value ::= scc
      }
      min
    }
  }
}
