package aoc2015.d8

import scala.io.Source

object d8 extends App {
  val input = Source.fromFile("8.txt").mkString

  val test = """""
                |"abc"
                |"aaa\"aaa"
                |"\x27"""".stripMargin

  // first part

  def count(str: List[Char]): Int = str match {
    case a :: b :: n1 :: n2 :: rest
        if a == '\\' && b == 'x' &&
          List(n1, n2).forall(_.toString.matches("[0-9a-f]")) =>
      3 + count(rest)
    case a :: b :: rest if a == '\\' && (List('\"', '\\').contains(b)) => 1 + count(rest)
    case x :: rest if x == '\"'                                        => 1 + count(rest)
    case x :: rest                                                     => 0 + count(rest)
    case Nil                                                           => 0
  }

  def run(input: String, countF: List[Char] => Int) = input.split('\n').map(s => countF(s.toList)).sum

  //println(run(input, count))

  // second part

  def count2(str: List[Char]): Int = str match {
    case a :: b :: n1 :: n2 :: rest
        if a == '\\' && b == 'x' &&
          List(n1, n2).forall(_.toString.matches("[0-9a-f]"))          => 1 + count2(rest)
    case a :: b :: rest if a == '\\' && (List('\"', '\\').contains(b)) => 2 + count2(rest)
    case x :: rest if x == '\"'                                        => 2 + count2(rest)
    case x :: rest                                                     => 0 + count2(rest)
    case Nil                                                           => 0
  }
  //println(test)
  println(run(input, count2))
}
