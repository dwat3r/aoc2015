package aoc2015.d9

import scala.io.Source

case class Edge(from: String, to: String, dist: Int)

object d9 extends App {
  val input = Source.fromFile("9.txt").mkString

  val test = """London to Dublin = 464
               |London to Belfast = 518
               |Dublin to Belfast = 141""".stripMargin

  type World = Map[String, Map[String, Int]]

  // part 1
  def parse(input: String): World = input.split("\n").foldLeft(Map.empty[String, Map[String, Int]]) { (map, line) =>
    val path = raw"(\w+) to (\w+) = (\d+)".r.anchored
    line match {
      case path(from, to, dist) => map + (from -> (map.getOrElse(to, Map.empty) + (to -> dist.toInt)))
    }
  }

  def run(input: World) = {
    def go(input: World, sum: Int): Int = 
      input.foldLeft(sum) { case (sum, (from, tos)) => 
        tos
    }
  }

  println(run(parse(test)))
}
