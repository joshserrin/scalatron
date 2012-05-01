package scalatron.botwar.botPlugin

import scala.collection.parallel.ParSeq
import scala.annotation.tailrec

class ControlFunctionFactory {
  def create: (String => String) = JoshBot.respond
}
object JoshBot {
  val MasterName = "JoshSerrin" // this requires ScalatronBot.jar to be placed into $SCALATRON/bots/JoshSerrin/

  val Welcome = """Welcome\(name=(.+),path=(.+),apocalypse=(\d+),round=(\d+)\)""".r
  val React = """React\(entity=(.*),time=(\d+),view=(.+),energy=(.+)\)""".r
  val ReactBot = """React\(entity=(.*),time=(\d+),energy=(.*),dx=(.*),dy=(.*),view=(.*)\)""".r
  val Goodbye = """Goodbye\(energy=(.+)\)""".r

  private val bots = Seq(GameKeeper, MasterBot, MiniBot)
  def respond(worldState: String): String =
    bots.map(_.respond(worldState)).filterNot(_.isEmpty).mkString("|")

  trait Bot {
    def respond(worldState: String): String
  }
  object GameKeeper extends Bot {
    private var name: String = ""
    var apocalypse: Int = 0
    override def respond(worldState: String): String = worldState match {
      case Welcome(name, path, apocalypse, round) => {
        this.name = name
        this.apocalypse = apocalypse.toInt
        ""
      }
      case Goodbye(energy) => println("Game ended.  %s ended with %s energy!".format(name, energy)); ""
      case _ => "" // do nothing
    }
  }
  def proximityToBadGuysFactor(view: View, cell: Cell)(implicit isEnemy: Cell => Boolean): Double = {
    type Distance = Double
    val closestEnemyLocation = view.cells.foldLeft[Option[(Cell, Distance)]](None) {
      case (currentClosest, next) => {
        def isFriendly = !isEnemy(next)
        if (isFriendly) currentClosest
        else {
          val dist = distance(view.whereIAm, next)
          currentClosest match {
            case None => Some((next, dist))
            case Some((_, d)) if dist < d => Some((next, dist))
            case _ => currentClosest
          }
        }
      }
    }
    closestEnemyLocation match {
      case None => 1 // nothing close to me so points are raw points
      case Some((_, distance)) => 1 / distance // 
    }
  }
  object MasterBot extends Bot {
    private val explorer: Explorer = new Explorer
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, viewString, energy) => {
        val view = View(viewString)
        Hunter(pointsCalc(view)).move(view) match {
          case Some(moveTo) => moveTo.toResponse
          case _ => {
            // nothing of interest to hunt towards.  Move to explorer
            explorer.move(view).map(_.toResponse).getOrElse("Status(text=Unknown)")
          }
        }
      }
      case _ => ""
    }
    private def pointsCalc(view: View)(cell: Cell): Double = {
      val basePoints: Double = {
        import cell._
        if (!isAccessible) Double.NegativeInfinity // Can't reach it, maybe blocked by walls?
        else if (isMyMiniBot) -100 // mini-bot disappears, energy added to bot
        else if (isWall) -10 // bonk, stunned for 4 cycles, loses 10 EU
        else if (isEmpty) 1 // small benefit to move // FIXME this will cause us to bounce around.  If nothing else is available, intelligently move to some random location!
        else if (isZugar) 100 // +100, plant disappears
        else if (isFluppet) 200 // +200, beast disappears
        else if (isEnemyMiniBot) -150 // +150, mini-bot disappears
        else if (cell.isEnemy) -200 // bonk or minibot can be killed!
        else if (isToxifera) -100 // -100, plant disappears
        else if (isSnorg) -150 //-150, bonk, beast also damaged
        else if (isMaster) 0
        else 0 // isUnknown
      }
      basePoints * proximityToBadGuysFactor(view, cell)
    }
    implicit def isEnemy(cell: Cell): Boolean = cell.isEnemy || cell.isEnemyMiniBot || cell.isSnorg
  }
}