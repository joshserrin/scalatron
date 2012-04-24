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
case class Navigator(view: View, pointsCalc: Cell => Double) {
  import view._
  type Fitness = Double
  implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
    override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
  }
  def bestMove: MoveTo = {
    val cellsWithBenefit = cells.filter(pointsCalc(_) > 0)
    if (cellsWithBenefit.isEmpty) {
      // TODO: something more sophisticated than a random jump
      // Shuffle so that we don't always value certain quadrants
      def furthestFirst(a: Cell, b: Cell): Boolean = distance(whereIAm, a) > distance(whereIAm, b)
      val destination = cells.filter(_.isAccessible).shuffle.sortWith(furthestFirst)(0)
      //      println("Explore")
      Explore(stepTowards(destination))
    } else {
      val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
      val (destination, withHighestPayoff) = cellsWithFitness.max
      //      println("Hunt")
      Hunt(stepTowards(destination))
    }
  }
  private def stepTowards(destination: Cell): Cell = {
    // theoretically this could return null but I don't think that will happen 
    // given that the graph should only consist of accessible points
    import org.jgrapht.alg._
    import scala.collection.JavaConverters._
    //    println("Calculating best path from %s to %s".format(whereIAm, destination))
    val bestPath = DijkstraShortestPath.findPathBetween(graph, whereIAm, destination)
//    println("bestPath: " + bestPath)
    val Edge(src, dest) = bestPath.asScala(0)
    dest
  }
  private def fitness(cell: Cell): Fitness = {
    // should also take into consideration the distance between the bot and the cell
    def distance: Double = euclideanDistance((0, 0), (cell.dx, cell.dy))
    def normalized(points: Double): Double = points / distance
    normalized(pointsCalc(cell))
  }
}