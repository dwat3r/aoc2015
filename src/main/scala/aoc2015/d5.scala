package aoc2015.d5

import scala.io.Source
import scala.annotation.tailrec

object d5 extends App {

  val input = Source.fromFile("5.txt").mkString

  val test = List(
    "ugknbfddgicrmopn",
    "aaa",
    "jchzalrnumimnmhp",
    "haegwjzuvuyypxyu",
    "dvszwmarrgswjxmb"
  )

// first part

  def nice(s: String): Boolean = {
    s.filter("aeiou".toList.contains(_)).length >= 3 &&
    ('a' to 'z').exists(c => s.containsSlice(s"${c}${c}")) &&
    !List("ab", "cd", "pq", "xy").exists(s.containsSlice(_))
  }

  def run(input: String) = input.split("\n").filter(nice).length

//println(run(input))

// second part
  val test2 = List(
    "qjhvhtzxzqqjkmpb",
    "xxyxx",
    "uurcxstgmygtbstg",
    "ieodomkazucvgmuy"
  )

  @tailrec
  final def two(s: String, rem: String): Boolean = {
    if (rem.length < 2) return false
    else if (s.indexOf(rem.take(2)) > -1) {
      val first = s.indexOf(rem.take(2))
      if (s.drop(first + 2).indexOf(rem.take(2)) > -1) return true
    }
    two(s, rem.drop(1))
  }

  @tailrec
  final def three(rem: String): Boolean = rem.toList match {
    case _ if (rem.length < 3)          => false
    case c :: _ :: c2 :: _ if (c == c2) => true
    case _                              => three(rem.drop(1))
  }

  def nice2(s: String): Boolean = {
    val ret = two(s, s) && three(s)
    ret
  }

  def run2(input: String) = input.split("\n").filter(_.nonEmpty).filter(nice2).length
  println((run2(input)))
}
