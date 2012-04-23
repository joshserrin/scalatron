package scalatron.botwar.botPlugin

/**
 * This is basically a 2D space where the MasterBot's location is the center
 * (0,0).  All cells are deltas from the master's location.
 */
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
      column <- 0 to rowString.length - 1
      cellContents = rowString.charAt(column).toString
      dx = delta(column)
    } yield (new Cell(cellContents, dx, dy, this))
  }
  val whereIAm: Cell = cells.find(c => c.dx == 0 && c.dy == 0).get
  def canMoveTo(dx: Int, dy: Int): Boolean =
    cells.find(cell => cell.dx == dx && cell.dy == dy).map(_.isAccessible).getOrElse(false)

  import org.jgrapht._
  import org.jgrapht.graph._
  case class Edge(src: Cell, dest: Cell) extends DefaultWeightedEdge {
    override def getWeight: Double = 1 // Nonsense weight for now
  }
  val graph: Graph[Cell, Edge] = {
    class MyEdgeFactory extends EdgeFactory[Cell, Edge] {
      override def createEdge(src: Cell, dest: Cell): Edge = new Edge(src, dest)
    }
    val graph: WeightedGraph[Cell, Edge] = new DirectedWeightedMultigraph(new MyEdgeFactory)
    def addAllVertices: Unit = cells.foreach(graph.addVertex(_))
    def addAllEdges: Unit = {
      def neighborsOf(cell: Cell): Seq[Cell] = {
        def isNeighbor(potentialNeighbor: Cell): Boolean = {
          def withinOneStep: Boolean = {
            val (ax, ay) = (cell.dx, cell.dy)
            val (bx, by) = (potentialNeighbor.dx, potentialNeighbor.dy)
            (ax - bx).abs <= 1 && (ay - by).abs <= 1
          }
          potentialNeighbor.isAccessible && withinOneStep
        }
        cells.filter(isNeighbor)
      }
      for {
        v <- cells
        neighbor <- neighborsOf(v)
        _ = graph.addEdge(v, neighbor)
      } yield ()
    }
    addAllVertices
    addAllEdges
    graph
  }

}