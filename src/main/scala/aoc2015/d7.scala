package aoc2015.d7

import scala.io.Source
import scala.collection.mutable

sealed trait Instr
case class Binary(op: (Short, Short) => Short, l: String, r: String) extends Instr
case class Unary(op: Short => Short, r: String)                      extends Instr
case class Val(v: Short)                                             extends Instr
case class Ref(l: String)                                            extends Instr

object d7 extends App {
  val input = Source.fromFile("7.txt").mkString

  val test = """123 -> x
               |456 -> y
               |x AND y -> d
               |x OR y -> e
               |x LSHIFT 2 -> f
               |y RSHIFT 2 -> g
               |NOT x -> h
               |NOT y -> i""".stripMargin('|')

  // first part

  implicit class RegexOps(sc: StringContext) {
    def r = new util.matching.Regex(
      sc.parts.mkString,
      sc.parts.tail.map(_ => "x"): _*
    )
  }

  def valuize(instr: Instr): Instr = instr match {
    case Binary(op, l, r) =>
      (l, r) match {
        case (r"(\d+)$l", r"([a-z]+)$r") => Unary(op(l.toShort, _), r)
        case (r"([a-z]+)$l", r"(\d+)$r") => Unary(op(_, r.toShort), l)
        case (r"(\d+)$l", r"(\d+)$r")    => Val(op(l.toShort, r.toShort))
        case _                           => instr
      }
    case Unary(op, r) =>
      r match {
        case r"(\d+)$r" => Val(op(r.toShort))
        case _          => instr
      }
    case i => i
  }

  def parse(input: String): Map[String, Instr] = input
    .split("\n")
    .map { line =>
      val Array(src, dest) = line.split(" -> ")
      val instr = src match {
        case r"(\d+)$d"                    => Val(d.toShort)
        case r"([a-z]+)$l"                 => Ref(l)
        case r"([\w]+)$l AND ([\w]+)$r"    => Binary((a, b) => (a & b).toShort, l, r)
        case r"([\w]+)$l OR ([\w]+)$r"     => Binary((a, b) => (a | b).toShort, l, r)
        case r"([\w]+)$l LSHIFT (\d+)$r"   => Unary(a => (a << r.toShort).toShort, l)
        case r"([\w]+)$l RSHIFT ([\w]+)$r" => Unary(a => (a >> r.toShort).toShort, l)
        case r"NOT (\w+)$r"                => Unary(a => (~a).toShort, r)
      }
      (dest, valuize(instr))
    }
    .toMap

  val memo = mutable.Map[String, Short]()
  def eval(value: String, input: Map[String, Instr]): Short = {
    memo.getOrElse(
      value, {
        val ret = input
          .get(value)
          .map({
            case Ref(l)           => eval(l, input)
            case Val(v)           => v
            case Unary(op, r)     => op(eval(r, input))
            case Binary(op, l, r) => op(eval(l, input), eval(r, input))
          })
          .get
        memo.put(value, ret)
        ret
      }
    )
  }

  // val m = parse(test)
  // println(m.map{case (v,_)=> v -> eval(v, m)})
  //println(parse(input))
  //println(eval("a", parse(input)))

  // second part

  val m = parse(input)
  val a = eval("a", parse(input))
  memo.clear()
  println(eval("a", m + ("b" -> Val(a))))
}
