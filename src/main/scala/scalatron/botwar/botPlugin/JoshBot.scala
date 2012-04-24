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

  private val bots = Seq(MasterBot, MissileLauncher, Missile)
  def respond(worldState: String): String =
    bots.map(_.respond(worldState)).filterNot(_.isEmpty).mkString("|")

  trait Bot {
    def respond(worldState: String): String
  }
  object MasterBot extends Bot {
    /**
     * the number of EU's to be received if the Master lands on this cell
     */
    private def pointsCalc(cell: Cell): Double = {
      import cell._
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
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, view, energy) =>
        Navigator(View(view), pointsCalc).bestMove.toResponse
      case _ => ""
    }
  }
  object MissileLauncher extends Bot {
    val useEnergy = 100
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, viewString, energy) if energy.toInt > 100 =>
        // If we have enough points to spawn a Missile and we have something
        // to attack, launch a missile!
        View(viewString).cells.filter(Missile.isAttackable)
          .map(Missile.response(_, useEnergy)).mkString("|")
      case _ => ""
    }
  }
  object Missile extends Bot {
    val ID = "Missile"
    def response(enemyLocation: Cell, energy: Int): String = {
      import enemyLocation._
      "Spawn(dx=%s,dy=%s,name=%s,energy=%s)".format(dx, dy, ID, energy)
    }
    def isAttackable(cell: Cell): Boolean =
      cell.isEnemy || cell.isEnemyMiniBot || cell.isSnorg
    private def pointsCalc(cell: Cell): Double = {
      import cell._
      if (!isAccessible) Double.NegativeInfinity
      else if (isEnemy) 100 // we want to go towards enemies to explode!
      else if (isSnorg) 50
      else if (isWall) -10
      else 0
    }
    override def respond(worldState: String): String = worldState match {
      case ReactBot(entity, time, energy, dx, dy, viewString) if entity == ID => {
        val view = View(viewString)
        withinExplosionProximityToEnemy(view) match {
          case Some(distance) => Explode(distance.toInt).toResponse
          case None => Navigator(view, pointsCalc).bestMove.toResponse
        }
      }
      case _ => ""
    }
    private def withinExplosionProximityToEnemy(view: View): Option[Double] = {
      import view._
      val enemies = cells.filter(isAttackable)
      if (enemies.isEmpty) None
      else {
        implicit val ProximityOrdering = new Ordering[Cell] {
          override def compare(a: Cell, b: Cell): Int = 0
        }
        val closestEnemy = enemies.min
        val dist = distance(whereIAm, closestEnemy)
        if (dist < 3) Some(dist)
        else None
      }
    }
  }
}