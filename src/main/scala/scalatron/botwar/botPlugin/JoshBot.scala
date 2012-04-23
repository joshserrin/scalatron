package scalatron.botwar.botPlugin

import scala.collection.parallel.ParSeq
import scala.annotation.tailrec

class ControlFunctionFactory {
  def create: (String => String) = JoshBot.respond
}
object JoshBot {
  val MasterName = "JoshSerrin" // this requires ScalatronBot.jar to be placed into $SCALATRON/bots/JoshSerrin/

  val Welcome = """Welcome\(name=(.+),path=(.+),apocalypse=(\d+),round=(\d+)\)""".r
  val React = """React\(entity=(.+),time=(\d+),view=(.+),energy=(.+)\)""".r
  val ReactBot = """React\(entity=(.+),time=(\d+),view=(.+),energy=(\d+),dx=(\d+),dy=(\d+)\)""".r
  val Goodbye = """Goodbye\(energy=(.+)\)""".r

  val bots: ParSeq[Bot] = Seq(GameKeeper, new MasterBot).par

  def respond(worldState: String): String =
    bots.map(_.respond(worldState)).reduce((a, b) =>
      if (a.isEmpty) b
      else if (b.isEmpty) a
      else a + "|" + b)

  trait Bot {
    def respond(worldState: String): String
  }
  object GameKeeper extends Bot {
    var apocalypse: Option[Int] = None
    var round: Option[Int] = None
    override def respond(worldState: String): String = worldState match {
      case Welcome(_, _, apocalypse, round) => {
        this.apocalypse = Some(apocalypse.toInt)
        this.round = Some(round.toInt)
        ""
      }
      case _ => "" // Don't care about anything else
    }
  }
  case class MasterBot extends Bot {
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, view, energy) => {
        val navigator = Navigator(View(view))
        val move = navigator.bestMove
        println(move)
        move.toResponse
      }
      case _ => ""
    }
  }
  case class View(view: String) {
    val cells: Seq[Cell] = {
      val rowLength = Math.sqrt(view.length).intValue
      val halfRowLength = rowLength / 2
      def delta(i: Int): Int = if (i < halfRowLength) -(halfRowLength - i) else i - halfRowLength
      for {
        row <- 0 to rowLength - 1
        dy = delta(row)
        startRowIndex = row * rowLength
        endRowIndex = startRowIndex + rowLength - 1
        rowString = view.substring(startRowIndex, endRowIndex)
        //        _ = println(rowString)
        column <- 0 to rowString.length - 1
        cellContents = rowString.charAt(column).toString
        dx = delta(column)
        _ = println("%s,%s turned into %s,%s".format(row, column, dy, dx))
      } yield (new Cell(cellContents, dx, dy))
    }
    //    cells.foreach(c => println(c.dx + "," + c.dy))
  }
  // dx and dy are the deltas to move from the MasterBot's position (0, 0)
  case class Cell(s: String, dx: Int, dy: Int) {
    val points: Int = {
      if (!isAccessible) Double.NegativeInfinity // Can't reach it, maybe blocked by walls?
      if (isMyMiniBot) 0 // mini-bot disappears, energy added to bot
      else if (isWall) -10 // bonk, stunned for 4 cycles, loses 10 EU
      else if (isEmpty) 1 // small benefit to move
      else if (isZugar) 100 // +100, plant disappears
      else if (isFluppet) 200 // +200, beast disappears
      else if (isEnemyMiniBot) 150 // +150, mini-bot disappears
      else if (isEnemy) 0 // bonk
      else if (isToxifera) -100 // -100, plant disappears
      else if (isSnorg) -150 //-150, bonk, beast also damaged
      else if (isMaster) 0
      else 0 // isUnknown
    }
    def isAccessible = true // TODO
    def isMyMiniBot = s == "S"
    def isWall = s == "W"
    def isEmpty = s == "_"
    def isZugar = s == "P"
    def isFluppet = s == "B"
    def isEnemyMiniBot = s == "s"
    def isEnemy = s == "m"
    def isToxifera = s == "p"
    def isSnorg = s == "b"
    def isMaster = s == "M"
    def isUnknown = s == "?"
  }
  case class Move(cell: Cell) {
    def toResponse: String = "Move(dx=%s,dy=%s)".format(cell.dx, cell.dy)
  }
  case class Navigator(view: View) {
    type Fitness = Double
    implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
      override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
    }
    def bestMove: Move = {
      val cellsWithBenefit = view.cells.withFilter(_.points > 0)
      val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
      val (cell, highestPayoff) = cellsWithFitness.max
      Move(cell)
    }
    private def fitness(cell: Cell): Fitness = {
      // should also tak into consideration the distance between the bot and the cell
      def euclideanDistance: Double = {
        val (p1, p2) = (0, 0)
        val (q1, q2) = (cell.dx, cell.dy)
        implicit def squared(d: Int) = new { def squared: Int = d * d }
        Math.sqrt((p1 - q1).squared + (p2 - q2).squared)
      }
      def normalized(points: Int): Double = points / euclideanDistance
      normalized(cell.points)
    }
  }

}