package llms.gen

import java.util
import java.util.IdentityHashMap

import llms.expressions.Expressions
import llms.util.GraphUtil

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.language.postfixOps

trait Scheduling {

  val IR: Expressions

  import IR._

  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    val scopeIndex = buildScopeIndex(scope)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(syms(result), scopeIndex), t => scheduleDepsWithIndex(syms(infix_rhs(t)), scopeIndex))
    if (sort) xx.foreach { x =>
      if (x.length > 1) {
        import llms.util.Utils._
        printerr("warning: recursive schedule for result " + result + ": " + x)
        new Exception printStackTrace()
      }
    }
    xx.flatten.reverse
  }

  protected def buildScopeIndex(scope: List[Stm]): util.IdentityHashMap[Sym[Any], (Stm, Int)] = {
    val cache = new util.IdentityHashMap[Sym[Any], (Stm, Int)]
    var idx = 0
    for (stm <- scope) {
      for (s <- infix_lhs(stm)) cache.put(s, (stm, idx)) //remember the original order of the stms
      idx += 1
    }
    cache
  }

  protected def scheduleDepsWithIndex(syms: List[Sym[Any]], cache: util.IdentityHashMap[Sym[Any], (Stm, Int)]): List[Stm] = {

    val sortedSet = new java.util.TreeSet[(Stm, Int)](
      (a: (Stm, Int), b: (Stm, Int)) => if (b._2 < a._2) -1 else if (b._2 == a._2) 0 else 1
    )

    for (sym <- syms) {
      val stm = cache.get(sym)
      if (stm ne null) sortedSet.add(stm)
    }

    var res: List[Stm] = Nil
    val iter = sortedSet.iterator //return stms in the original order given by 'scope'
    while (iter.hasNext) {
      res ::= iter.next._1
    }
    res
  }

  def getFatDependentStuff(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    if (sts.isEmpty) return Nil
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */

    //type IdentityHashMap[K,V] = HashMap[K,V]

    // IdentityHashMap appears faster than scala.collection.mutable.HashMap here (based on perf. testing)
    // possible improvement: use an integer hashmap that works directly with sym ids

    val lhsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val symsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val boundSymsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    //val boundSymsCache = new IdentityHashMap[Sym[Any], Set[Stm]]()

    def infix_getOrElse[K, V](map: IdentityHashMap[K, V], s: K, f: => V) = {
      var res = map.get(s) //map(s)
      if (res == null) res = f
      res
    }

    def putDef(map: IdentityHashMap[Sym[Any], List[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s) //map(s)
      if (res == null) res = Nil
      //map.getOrElse(s, Nil) match {
      res match {
        case `d` :: ds =>
        case ds => map.update(s, d :: ds) //map.put(s,d::ds)
      }
    }

    def putDefSet(map: IdentityHashMap[Sym[Any], Set[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map(s) //map.get(s)
      if (res == null) {
        res = Set[Stm]()
        map.update(s, res) //map.put(s,res)
      }
      res += d
    }

    for (d <- scope) {
      infix_lhs(d).foreach(s => putDef(lhsCache, s, d))
      syms(infix_rhs(d)).foreach(s => putDef(symsCache, s, d))
      boundSyms(infix_rhs(d)).foreach(st => putDef(boundSymsCache, st, d))
      tunnelSyms(infix_rhs(d)).foreach(st => putDef(boundSymsCache, st, d)) // treat tunnel like bound
    }

    /*
    optimization:
      traverse syms by ascending id. if sym s1 is used by s2, do not evaluate further
      uses of s2 because they are already there.

    CAVEAT: TRANSFORMERS !!!

    assumption: if s2 uses s1, the scope of s2 is completely included in s1's scope:

      val A = loop { s1 => ... val B = sum { s2 => ... val y = s2 + s1; .../* use y */ ... } }

      once we reach y the second time (from s2) we can stop, because the uses of
      y have been tracked up to A, which includes all of B
    */

    val seen = new mutable.HashSet[Sym[Any]]

    def getDepStuff(st: Sym[Any]) = {
      // could also precalculate uses, but computing all combinations eagerly is also expensive
      def uses(s: Sym[Any]): List[Stm] = if (seen(s)) Nil else {
        //seen += s
        lhsCache.getOrElse(s, Nil) ::: symsCache.getOrElse(s, Nil) filterNot (boundSymsCache.getOrElse(st, Nil) contains _)
      }

      GraphUtil.stronglyConnectedComponents[Stm](
        uses(st),
        t => infix_lhs(t) flatMap uses
      ).flatten
    }

    /*
    reference impl:*/
    val res = sts.flatMap(getDepStuff).distinct

    /*if (sts.contains(Sym(1064))) {
      println("dep on x1064:")
      res.foreach { r =>
        println("   " + r)
      }
    }*/
    res

    // CAVEAT: TRANSFORMERS !!!  see CloseWorldRestage app in Delite
    //sts.sortBy(_.id).flatMap(getDepStuff)
  }
}
