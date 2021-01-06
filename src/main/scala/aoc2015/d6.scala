package aoc2015

import scala.io.Source
import cats.implicits._

case class Pos(x: Int, y: Int)
case class Instr(instr: String, p1: Pos, p2: Pos)

object d6 extends App {
  val input = Source.fromFile("6.txt").mkString

  val test = """|turn on 0,0 through 999,999
                |toggle 0,0 through 999,0
                |turn off 499,499 through 500,500""".stripMargin('|')

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
    val setting = input.foldLeft(Map[Pos, Boolean]()) { case (states, Instr(instr, p1, p2)) =>
      instr match {
        case "turn on"  => states ++ List((p1, true), (p2, true))
        case "turn off" => states ++ List((p1, false), (p2, false))
        case "toggle" =>
          states ++ List(
            (p1, !states.getOrElse(p1, false)),
            (p2, !states.getOrElse(p2, false))
          )
      }
    }
   //setting
    ((0 to 999).toList, (0 to 999).toList).mapN(Pos.apply)
      .foldLeft((false,0)){ case ((st, acc), p) =>
        setting.get(p) match {
          case Some(nst) => if (nst) (nst, acc+1) else (nst, acc)
          case None      => if (st) (st, acc+1) else (st, acc)
        }
      }
  }

  println(run(parse(test)))//.mkString("\n"))
}
