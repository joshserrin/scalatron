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
  val ReactBot = """React\(entity=(.+),time=(\d+),energy=(.+),dx=(.+),dy=(.+),view=(.+)\)""".r
  val Goodbye = """Goodbye\(energy=(.+)\)""".r

  private val bots = Seq(GameKeeper, MasterBot)
  def respond(worldState: String): String =
    bots.map(_.respond(worldState)).filterNot(_.isEmpty).mkString("|")

  trait Bot {
    def respond(worldState: String): String
  }
  object GameKeeper extends Bot {
    private var name: String = ""
    override def respond(worldState: String): String = worldState match {
      case Welcome(name, path, apocalypse, round) => this.name = name; ""
      case Goodbye(energy) => println("Game ended.  %s ended with %s energy!".format(name, energy)); ""
      case _ => "" // do nothing
    }
  }
  def pointsCalc(cell: Cell): Double = {
    import cell._
    if (!isAccessible) Double.NegativeInfinity // Can't reach it, maybe blocked by walls?
    else if (isMyMiniBot) 0 // mini-bot disappears, energy added to bot
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
  object MasterBot extends Bot {
    private val explorer: Explorer = new Explorer
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, viewString, energy) => {
        val view = View(viewString)
        // TODO Eventually we want to go back to the Master so he can absorb our
        // points.  But when?
        Hunter(pointsCalc).move(view) match {
          case Some(moveTo) => moveTo.toResponse
          case _ => {
            // nothing of interest to hunt towards.  Move to explorer
            explorer.move(view).map(_.toResponse).getOrElse("Status(text=Unknown)")
          }
        }
      }
      case _ => ""
    }
  }
}