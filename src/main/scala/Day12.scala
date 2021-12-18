import Util.readFile

import scala.annotation.tailrec

@main def day12(): Unit = {
  val input: Map[String, List[String]] = readFile("resources/day12").map { x =>
    val split = x.split("-")
    split.head -> split.last
  }
    .flatMap((k, v) => List(k -> v, v -> k))
    .groupBy(_._1).map { (k, v) => k -> v.map(_._2) }

  def findPaths1(toExplore: List[String], xs: Map[String, List[String]], prefix: List[String]): List[List[String]] =
    toExplore.flatMap { v =>
      if (v == "end") List(prefix.appended(v))
      else if (xs.get(v).toList.flatten.isEmpty) List.empty
      else findPaths1(xs(v), if (v == v.toUpperCase) xs else xs.filterNot(_._1 == v), prefix.appended(v))
    }

  def findPaths2(
                  toExplore: List[String],
                  xs: Map[String, List[String]],
                  prefix: List[String],
                  smallCaveVisitedTwice: Boolean,
                ): List[List[String]] =
    toExplore.flatMap { v =>
      if (v == "end")
        List(prefix.appended(v))
      else if (xs.get(v).toList.flatten.isEmpty)
        List.empty
      else if (v == v.toUpperCase)
        findPaths2(xs(v), xs, prefix.appended(v), smallCaveVisitedTwice)
      else if (smallCaveVisitedTwice)
        findPaths2(xs(v), xs.filterNot(_._1 == v), prefix.appended(v), true)
      else
        findPaths2(xs(v), xs.filterNot(_._1 == v), prefix.appended(v), false)
          .appendedAll(findPaths2(xs(v), xs, prefix.appended(v), true))
    }

  println(findPaths1(input("start"), input.filterNot(_._1 == "start"), List("start")).length)
  println(findPaths2(input("start"), input.filterNot(_._1 == "start"), List("start"), false).distinct.length)
}