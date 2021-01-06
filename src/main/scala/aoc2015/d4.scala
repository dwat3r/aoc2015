package aoc2015

import scala.annotation.tailrec
import java.security.MessageDigest

object d4 extends App {
  val input = "yzbqklnj"
// part 1
  def md5(s: String): String =
    MessageDigest
      .getInstance("MD5")
      .digest(s.getBytes())
      .map("%02X".format(_))
      .mkString

  @tailrec
  final def mine(key: String, n: Int): Int = {
    //println(md5(s"${key}${n}"))
    if (md5(s"${key}${n}").startsWith("00000")) n
    else {
      mine(key, n + 1)
    }
  }

//md5("abcdef609043")
  println(mine(input, 0))

// part 2
  @tailrec
  final def mine2(key: String, n: Int): Int = {
    //println(md5(s"${key}${n}"))
    if (md5(s"${key}${n}").startsWith("000000")) n
    else {
      mine2(key, n + 1)
    }
  }

  println(mine2(input, 0))
}
