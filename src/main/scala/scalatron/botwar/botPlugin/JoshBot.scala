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
        //        println(move)
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
        //        _ = println("%s,%s maps to %s,%s".format(row, column, dy, dx))
      } yield (new Cell(cellContents, dx, dy, this))
    }
    //    cells.foreach(c => println(c.dx + "," + c.dy))
    def canMoveTo(dx: Int, dy: Int): Boolean =
      cells.find(cell => cell.dx == dx && cell.dy == dy).map(_.isAccessible).getOrElse(false)
  }
  // dx and dy are the deltas to move from the MasterBot's position (0, 0)
  case class Cell(s: String, dx: Int, dy: Int, view: View) {
    val points: Int = {
      if (!isAccessible) Double.NegativeInfinity // Can't reach it, maybe blocked by walls?
      if (isMyMiniBot) 0 // mini-bot disappears, energy added to bot
      else if (isWall) -10 // bonk, stunned for 4 cycles, loses 10 EU
      else if (isEmpty) 0 // small benefit to move // FIXME this will cause us to bounce around.  If nothing else is available, intelligently move to some random location!
      else if (isZugar) 100 // +100, plant disappears
      else if (isFluppet) 200 // +200, beast disappears
      else if (isEnemyMiniBot) 150 // +150, mini-bot disappears
      else if (isEnemy) 0 // bonk
      else if (isToxifera) -100 // -100, plant disappears
      else if (isSnorg) -150 //-150, bonk, beast also damaged
      else if (isMaster) 0
      else 0 // isUnknown
    }
    def isAccessible = true //!isWall || !isEnemy
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
  case class MoveTo(cell: Cell) {
    def toResponse: String = {
      val (currentX, currentY) = (0.0, 0.0)
      def calculateSlope(destX: Int, destY: Int): Double = {
        val deltaX = destX - currentX
        val deltaY = currentY - destY // remember, Y is inverted!
        if (deltaX != 0) deltaY / deltaX else if (deltaY > 0) 1 else -1
      }
      val slope = calculateSlope(cell.dx, cell.dy)
      val (destX, destY) = (cell.dx, cell.dy)
      // If I needed to move in a certan direction which two points are accessible (at least my own location of 0)
      def possibilities(current: Double, dest: Double) = if (current > dest) List(0, -1) else List(0, 1)
      val moveXPossibilities = possibilities(currentX, destX)
      val moveYPossibilities = possibilities(currentY, destY)
      // The (three) possible moves that I could make taking out of consideration sitting still.
      val possibleMoves: List[(Int, Int)] = {
        for {
          x <- moveXPossibilities
          y <- moveYPossibilities
        } yield ((x, y))
      } filterNot { case (x, y) => x == 0 && y == 0 }
      def closestToSlope(a: (Int, Int), b: (Int, Int)): Boolean = {
        val slopeA = calculateSlope(a._1, a._2)
        val slopeB = calculateSlope(b._1, b._2)
        val deltaA = (slopeA - slope).abs
        val deltaB = (slopeB - slope).abs
        deltaA < deltaB
      }
      // Order the moves so that the first is closest to the slope that I am supposed to travel
      val orderedMoves = possibleMoves.sortWith(closestToSlope)
      // The best move is the first move closest to my desired slope that I can actually move it
      val bestMove = orderedMoves find { case (dx, dy) => cell.view.canMoveTo(dx, dy) }
      val (moveX, moveY) = bestMove.get // assumes to get an answer.  if no answer do random walk?
      "Move(dx=%s,dy=%s)".format(moveX, moveY)
    }
  }
  case class Navigator(view: View) {
    type Fitness = Double
    implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
      override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
    }
    def bestMove: MoveTo = {
      val cellsWithBenefit = view.cells.filter(_.points > 0)
      if (cellsWithBenefit.isEmpty) {
        // do random walk!
        implicit def shuffle(l: Seq[Cell]) = new {
          def shuffle: Seq[Cell] = {
            import scala.collection.JavaConverters._
            val cells = new java.util.ArrayList[Cell](l.asJava)
            java.util.Collections.shuffle(cells)
            cells.asScala
          }
        }
        implicit val FurthestCell = new Ordering[Cell] {
          def compare(a: Cell, b: Cell) = {
            val masterLoc = (0, 0)
            val aDist = euclideanDistance(masterLoc, (a.dx, a.dy))
            val bDist = euclideanDistance(masterLoc, (b.dx, b.dy))
            aDist.compareTo(bDist)
          }
        }
        val randomCell = view.cells.filter(_.isAccessible).shuffle.max
        MoveTo(randomCell)
      } else {
        val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
        val (cell, highestPayoff) = cellsWithFitness.max
        MoveTo(cell)
      }
    }
    private def fitness(cell: Cell): Fitness = {
      // should also tak into consideration the distance between the bot and the cell
      def distance: Double = euclideanDistance((0, 0), (cell.dx, cell.dy))
      def normalized(points: Int): Double = points / distance
      normalized(cell.points)
    }
  }

}