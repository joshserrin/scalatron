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
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, view, energy) => Navigator(View(view)).bestMove.toResponse
      case _ => ""
    }
  }
  object MissileLauncher extends Bot {
    val useEnergy = 100
    override def respond(worldState: String): String = worldState match {
      case React(entity, time, viewString, energy) if energy.toInt > 100 =>
        View(viewString).cells.filter(_.isEnemy).map(Missile.response(_, useEnergy)).mkString("|")
      case _ => ""
    }
  }
  object Missile extends Bot {
    val ID = "Missile"
    def response(enemyLocation: Cell, energy: Int): String = {
      import enemyLocation._
      "Spawn(dx=%s,dy=%s,name=%s,energy=%s)".format(dx, dy, ID, energy)
    }

    override def respond(worldState: String): String = worldState match {
      case ReactBot(entity, time, energy, dx, dy, view) if entity == ID => {
        // Find closest thing to blow up and blow it up!
        ""
      }
      case _ => ""
    }
  }
  trait MoveTo {
    def toResponse: String
  }
  case class Explore(cell: Cell) extends MoveTo {
    override def toResponse = "Move(dx=%s,dy=%s)|Status(text=Exploring)".format(cell.dx, cell.dy)
  }
  case class Hunt(cell: Cell) extends MoveTo {
    override def toResponse = "Move(dx=%s,dy=%s)|Status(text=Hunting)".format(cell.dx, cell.dy)
  }
  /**
   * Determines where to go to next.  First the highest values and closest items
   * are considered.  If nothing is value is seen this tries to explore the area.
   */
  case class Navigator(view: View) {
    import view._
    type Fitness = Double
    implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
      override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
    }
    def bestMove: MoveTo = {
      val cellsWithBenefit = cells.filter(_.points > 0)
      if (cellsWithBenefit.isEmpty) {
        // TODO: something more sophisticated than a random jump
        // Shuffle so that we don't always value certain quadrants
        def furthestFirst(a: Cell, b: Cell): Boolean = distance(whereIAm, a) > distance(whereIAm, b)
        val destination = cells.filter(_.isAccessible).shuffle.sortWith(furthestFirst)(0)
        Explore(stepTowards(destination))
      } else {
        val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
        val (destination, withHighestPayoff) = cellsWithFitness.max
        Hunt(stepTowards(destination))
      }
    }
    private def stepTowards(destination: Cell): Cell = {
      // theoretically this could return null but I don't think that will happen 
      // given that the graph should only consist of accessible points
      import org.jgrapht.alg._
      import scala.collection.JavaConverters._
      val bestPath = DijkstraShortestPath.findPathBetween(graph, whereIAm, destination)
      val Edge(src, dest) = bestPath.asScala(0)
      dest
    }
    private def fitness(cell: Cell): Fitness = {
      // should also take into consideration the distance between the bot and the cell
      def distance: Double = euclideanDistance((0, 0), (cell.dx, cell.dy))
      def normalized(points: Int): Double = points / distance
      normalized(cell.points)
    }
  }

}