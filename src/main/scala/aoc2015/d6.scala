package aoc2015.d6

import scala.io.Source
import cats.implicits._

case class Pos(x: Int, y: Int)
case class Instr(instr: String, p1: Pos, p2: Pos)

object d6 extends App {
  val input = Source.fromFile("6.txt").mkString

  val test = """|turn on 0,0 through 999,999
                |toggle 0,0 through 999,0
                |turn off 499,499 through 500,500""".stripMargin
// part 1
  def parse(input: String) = {
    val line =
      raw"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)".r.anchored
    input
      .split("\n")
      .map(_ match {
        case line(instr, x1, y1, x2, y2) =>
          Instr(instr, Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
      })
  }

  def run(input: Array[Instr]) = {
    val states = Array.fill(1000)(Array.fill(1000)(false))
    input.foreach { case Instr(instr, p1, p2) =>
      val range = ((p1.x to p2.x).toList, (p1.y to p2.y).toList)
      instr match {
        case "turn on"  => range.mapN(states(_)(_) = true)
        case "turn off" => range.mapN(states(_)(_) = false)
        case "toggle"   => range.mapN((x, y) => states(x)(y) = !states(x)(y))
      }
    }
    states.flatten.filter(identity).length
  }

  println(run(parse(input)))

  // part 2
  def run2(input: Array[Instr]) = {
    val states = Array.fill(1000)(Array.fill(1000)(0))
    input.foreach { case Instr(instr, p1, p2) =>
      val range = ((p1.x to p2.x).toList, (p1.y to p2.y).toList)
      instr match {
        case "turn on"  => range.mapN((x, y) => states(x)(y) = states(x)(y) + 1)
        case "turn off" => range.mapN((x, y) => states(x)(y) = if (states(x)(y) - 1 < 0) 0 else states(x)(y) - 1)
        case "toggle"   => range.mapN((x, y) => states(x)(y) = states(x)(y) + 2)
      }
    }
    states.flatten.sum
  }

  println(run2(parse(input)))
}
