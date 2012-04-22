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
        move.toResponse
      }
      case _ => ""
    }
  }
  case class View(view: String) {
    val rowLength = Math.sqrt(view.length).intValue
    val worldView: List[List[String]] = view.sliding(rowLength, rowLength).map(_.toCharArray.map("" + _).toList).toList
  }
  case class Cell(dx: Int, dy: Int, view: View) {

  }
  case class Move(cell: Cell) {
    def toResponse: String = "Move(dx=%s,dy=%s)".format(cell.dx, cell.dy)
  }
  case class Navigator(view: View) {
    val me = view.rowLength / 2

    type Fitness = Double
    implicit val moveOrdering = new Ordering[(Move, Fitness)] {
      override def compare(a: (Move, Fitness), b: (Move, Fitness)) = a._2.compareTo(b._2)
    }
    implicit def shuffle(seq: Seq[(Move, Double)]) = new {
      def shuffle: Seq[(Move, Fitness)] = {
        import scala.collection.JavaConverters._
        val jArray = java.util.Arrays.asList(seq: _*)
        java.util.Collections.shuffle(jArray)
        jArray.asScala
      }
    }
    def bestMove: Move = allMoves.map(m => (m, fitness(m))).shuffle.max._1
    val outlook = view.rowLength / 2
    def allMoves: Seq[Move] =
      for {
        dx <- -outlook to outlook
        dy <- -outlook to outlook
      } yield (Move(Cell(dx, dy, view)))

    implicit def isMyMiniBot(s: String) = new { def isMyMiniBot = s == "S" }
    implicit def isWall(s: String) = new { def isWall = s == "W" }
    implicit def isUnknown(s: String) = new { def isUnknown = s == "?" }
    implicit def isEmpty(s: String) = new { def isEmptyCell = s == "_" }
    implicit def isZugar(s: String) = new { def isZugar = s == "P" }
    implicit def isFluppet(s: String) = new { def isFluppet = s == "B" }
    implicit def isEnemyMiniBot(s: String) = new { def isEnemyMiniBot = s == "s" }
    implicit def isEnemy(s: String) = new { def isEnemy = s == "m" }
    implicit def isToxifera(s: String) = new { def isToxifera = s == "p" }
    implicit def isSnorg(s: String) = new { def isSnorg = s == "b" }
    implicit def isMe(s: String) = new { def isMe = s == "M" }
    def fitness(move: Move): Fitness = {
      import move.cell
      val x = me + cell.dx
      val y = me + cell.dy
      def isOutOfView(z: Int) = z < 0 || z >= view.rowLength
      if (isOutOfView(x) || isOutOfView(y)) Double.NegativeInfinity // Can't move there!
      else {
        val cell = view.worldView(y)(x)
        if (cell.isUnknown) -10
        else if (cell.isEmptyCell) 0
        else if (cell.isWall) -10 //...a wall => bonk, stunned for 4 cycles, loses 10 EU
        else if (cell.isEnemy) -10 //...another player's master bot => bonk
        else if (cell.isMyMiniBot) 20 //...one of its own mini-bots => mini-bot disappears, energy added to bot
        else if (cell.isEnemyMiniBot) 150 //...another player's mini-bot => +150, mini-bot disappears
        else if (cell.isZugar) 100 //...a Zugar => +100, plant disappears
        else if (cell.isToxifera) -100 //...a Toxifera => -100, plant disappears
        else if (cell.isFluppet) 200 //...a Fluppet => +200, beast disappears
        else if (cell.isSnorg) -150 //...a Snorg => -150, bonk, beast also damaged\
        else if (cell.isMe) Double.NegativeInfinity // That's where I current am!
        else throw new IllegalArgumentException("Unknown cell: " + cell)
      }
    }
  }

}