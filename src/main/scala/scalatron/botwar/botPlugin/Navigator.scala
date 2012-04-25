package scalatron.botwar.botPlugin

trait MoveTo {
  def toResponse: String
}
case class Explore(cell: Cell) extends MoveTo {
  override def toResponse = "Move(dx=%s,dy=%s)|Status(text=Exploring)".format(cell.dx, cell.dy)
}
case class Hunt(cell: Cell) extends MoveTo {
  override def toResponse = "Move(dx=%s,dy=%s)|Status(text=Hunting)".format(cell.dx, cell.dy)
}
case class Explode(size: Int) {
  def toResponse = "Explode(size=%s)".format(size)
}
/**
 * Determines where to go to next.  First the highest values and closest items
 * are considered.  If nothing is value is seen this tries to explore the area.
 */
trait Navigator {
  type Fitness = Double
  implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
    override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
  }
  def move(view: View): Option[MoveTo]
  def stepTowards(view: View, destination: Cell): Option[Cell] = {
    // theoretically this could return null but I don't think that will happen 
    // given that the graph should only consist of accessible points
    import org.jgrapht.alg._
    import scala.collection.JavaConverters._
    val bestPath = DijkstraShortestPath.findPathBetween(view.graph, view.whereIAm, destination)
    if (null == bestPath) None
    else {
      val Edge(src, dest) = bestPath.asScala(0)
      Some(dest)
    }
  }
}
case class Hunter(pointsCalc: Cell => Double) extends Navigator {
  def move(view: View): Option[MoveTo] = {
    val cellsWithBenefit = view.cells.filter(pointsCalc(_) > 0)
    if (cellsWithBenefit.isEmpty) {
      None
    } else {
      val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
      val (destination, withHighestPayoff) = cellsWithFitness.max
      stepTowards(view, destination).map(Hunt(_))
    }
  }
  def fitness(cell: Cell): Fitness = {
    // should also take into consideration the distance between the bot and the cell
    def distance: Double = euclideanDistance((0, 0), (cell.dx, cell.dy))
    def normalized(points: Double): Double = points / distance
    normalized(pointsCalc(cell))
  }
}
/**
 * This is a very simple, non-optimal explorer based off of the Random mouse algorithm
 * described at http://en.wikipedia.org/wiki/Maze_solving_algorithm#Pledge_algorithm.
 * The goal is to head in a single direction until you can't head in that direction
 * anymore, and then switch directions.  As long as the bot is trying to find a
 * better move and uses this Explorer only when a good move isn't available this
 * should work better than random exploring which usually causes the bot to jump
 * around.  Also, as the state of the map changes I don't want to try and store
 * what I've seen to prevent myself from going there twice (using DFS or BFS).
 */
case class Explorer extends Navigator {
  // The direction that the bot will step towards.  If the step isn't available
  // try stepping in a different direction
  private var stepDirection: (Int, Int) = (1, 0)
  override def move(view: View): Option[MoveTo] = {
    val (dx, dy) = stepDirection
    view.cells.find(c => c.dx == dx && c.dy == dy) match {
      case Some(cell) if cell.isAccessible => Some(Explore(cell))
      case Some(cell) if !cell.isAccessible => {
        // change direction clockwise
        stepDirection = stepDirection match {
          // case (0, 0) => // should never happen!
          case (-1, 1) => (0, 1)
          case (1, -1) => (0, -1)
          case (-1, dy) => (-1, dy + 1) //(-1, 0) => (-1, 1) and (-1, -1) => (-1, 0)
          case (0, dy) => (dy, dy) // (0, -1) => (-1, -1) and (0, 1) => (1, 1)
          case (1, dy) => (1, dy - 1) // (1, 0) => (1, -1) and (1, 1) => (1, 0)
        }
        move(view)
      }
      case _ => throw new IllegalStateException("Should be able to find cell is single direction but did not")
    }
  }
}