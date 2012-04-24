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
case class Explorer extends Navigator {
  override def move(view: View): Option[MoveTo] = {
    // TODO: something more sophisticated than a random jump
    // Shuffle so that we don't always value certain quadrants
    def furthestFirst(a: Cell, b: Cell): Boolean =
      distance(view.whereIAm, a) > distance(view.whereIAm, b)
    val destination = view.cells.filter(_.isAccessible).shuffle.sortWith(furthestFirst)(0)
    stepTowards(view, destination).map(Explore(_))
  }
}