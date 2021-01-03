import scala.io.Source

val input = Source.fromFile("3.txt").mkString

def parse(input: String): Array[String] = {
  input.split("")
}
//first part
case class Pos(x: Int, y: Int)

def run(input: Array[String]) = {
  input.foldLeft(
    (Map(Pos(0, 0)->1), Pos(0, 0)))({ case ((poss, pos), dir) =>
    val npos = dir match {
      case "^" => Pos(pos.x, pos.y+1)
      case "v" => Pos(pos.x, pos.y-1)
      case "<" => Pos(pos.x-1, pos.y)
      case ">" => Pos(pos.x+1, pos.y)
    }
    (poss.updated(npos, poss.getOrElse(npos,0)+1),npos)
  })._1.size
}

run(parse(input))
// second part

val test = "^v"
val test2 = "^>v<"
val test3 = "^v^v^v^v^v"

def run2(input: Array[String]) = {
  val (santa, robo, _, _, _) = 
  input.foldLeft(
    (
      Map(Pos(0, 0)->1), 
      Map(Pos(0, 0)->1), 
      Pos(0, 0),
      Pos(0, 0),
      1)
    )({ case ((santaPoss, roboPoss, santaPos, roboPos, turn), dir) =>
      val pos = if(turn%2==1) santaPos else roboPos
      val npos = dir match {
      case "^" => Pos(pos.x, pos.y+1)
      case "v" => Pos(pos.x, pos.y-1)
      case "<" => Pos(pos.x-1, pos.y)
      case ">" => Pos(pos.x+1, pos.y)
    }
    val (nSantaPoss, nSantaPos) = if(turn%2==1) (santaPoss.updated(npos, santaPoss.getOrElse(npos,0)+1),npos) else (santaPoss, santaPos)
    val (nRoboPoss, nRoboPos)  = if(turn%2==0) (roboPoss.updated(npos, roboPoss.getOrElse(npos,0)+1), npos) else (roboPoss, roboPos)
    (nSantaPoss, nRoboPoss, nSantaPos, nRoboPos, turn+1)
  })
  (santa ++ robo).size
}

println(run2(parse(input)))
