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
  def proximityToBadGuysFactor(view: View, cell: Cell): Double = {
    type Distance = Double
    val closestEnemyLocation = view.cells.foldLeft[Option[(Cell, Distance)]](None) {
      case (currentClosest, next) => {
        def isEnemy = cell.isEnemy || cell.isEnemyMiniBot || cell.isSnorg
        def isFriendly = !isEnemy
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
  def pointsCalc(view: View)(cell: Cell): Double = {
    val basePoints: Double = {
      import cell._
      if (!isAccessible) Double.NegativeInfinity // Can't reach it, maybe blocked by walls?
      else if (isMyMiniBot) -100 // mini-bot disappears, energy added to bot
      else if (isWall) -10 // bonk, stunned for 4 cycles, loses 10 EU
      else if (isEmpty) 0 // small benefit to move // FIXME this will cause us to bounce around.  If nothing else is available, intelligently move to some random location!
      else if (isZugar) 100 // +100, plant disappears
      else if (isFluppet) 200 // +200, beast disappears
      else if (isEnemyMiniBot) -150 // +150, mini-bot disappears
      else if (isEnemy) -200 // bonk or minibot can be killed!
      else if (isToxifera) -100 // -100, plant disappears
      else if (isSnorg) -150 //-150, bonk, beast also damaged
      else if (isMaster) 0
      else 0 // isUnknown
    }
    basePoints * proximityToBadGuysFactor(view, cell)
  }
  object MasterBot extends Bot {
    private val explorer: Explorer = new Explorer
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, viewString, energy) => {
        val view = View(viewString)
        var moveString = Hunter(pointsCalc(view)).move(view) match {
          case Some(moveTo) => moveTo.toResponse
          case _ => {
            // nothing of interest to hunt towards.  Move to explorer
            explorer.move(view).map(_.toResponse).getOrElse("Status(text=Unknown)")
          }
        }
        val energyPerBot = 200
        def shouldMakeMiniBot: Boolean = energy.toInt > energyPerBot // we have some surplus past
        if (shouldMakeMiniBot) {
          // Assign 200 per minibots
          val botsToMake = 1 // make one at a time math.max((energy.toInt / energyPerBot), 1) // just make one for now!
          // TODO make sure the cell is value
          val cellsToPlace = for {
            dx <- -1 to 1
            dy <- -1 to 1
            if view.canMoveTo(dx, dy)
          } yield ((dx, dy))
          val spawnStrings = for {
            botID <- 0 to botsToMake
            if cellsToPlace.size > botID
            (dx, dy) = cellsToPlace(botID)
            name = "MiniBot-%s-%s".format(time, botID)
          } yield ("Spawn(dx=%s,dy=%s,name=%s,energy=%s)".format(dx, dy, name, energyPerBot))
          moveString = (moveString :: spawnStrings.toList).mkString("|")
        }
        moveString
      }
      case _ => ""
    }
  }
  object MiniBot extends Bot {
    import scala.collection.mutable.{ Map => mMap }
    private val explorerMap: mMap[String, Explorer] = mMap.empty
    override def respond(worldState: String): String = worldState match {
      case ReactBot(entity, time, energy, dx, dy, viewString) => {
        println("%s has %s energy".format(entity, energy))
        val view = View(viewString)
        val isHarvesting = (time.toInt + 250 < GameKeeper.apocalypse) || energy.toInt > 1000
        if (isHarvesting) {
          // TODO better pointsCalculation.  Keep away from MasterBot, Enemies, 
          // enemy minibots and other minibots
          val moveString = Hunter(pointsCalc(view)).move(view) match {
            case Some(moveTo) => moveTo.toResponse
            case _ => explore(entity, view)
          }
          moveString
        } else {
          println("%s is traveling to master!")
          // Travel back to MasterBot
          def pointsCalc(cell: Cell): Double = {
            val basePoints: Double = {
              if (cell.isMaster) 100
              else if (cell.isEnemy || cell.isEnemyMiniBot || cell.isMyMiniBot
                || cell.isSnorg || cell.isToxifera) -100
              else if (cell.isWall) -50
              else 0
            }
            basePoints * proximityToBadGuysFactor(view, cell)
          }
          Hunter(pointsCalc).move(view) match {
            case Some(moveTo) => moveTo.toResponse
            case _ => explore(entity, view)
          }
        }
      }
      case _ => ""
    }
    def explore(entity: String, view: View): String = {
      // nothing of interest to hunt towards.  Move to explorer
      val explorer = explorerMap.get(entity) match {
        case Some(explorer) => explorer
        case None => {
          val explorer = new Explorer
          explorerMap += (entity -> explorer)
          explorer
        }
      }
      explorer.move(view).map(_.toResponse).getOrElse("Status(text=Unknown)")
    }
  }
}