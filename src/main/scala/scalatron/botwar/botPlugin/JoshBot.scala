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
        println("Master told to respond")
        val navigator = Navigator(View(view))
        println("Navigator built!  Asking for best move")
        val move = navigator.bestMove
        move.toResponse
      }
      case _ => ""
    }
  }
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
        //        _ = println(rowString)
        column <- 0 to rowString.length - 1
        cellContents = rowString.charAt(column).toString
        dx = delta(column)
        //        _ = println("%s,%s maps to %s,%s".format(row, column, dy, dx))
      } yield (new Cell(cellContents, dx, dy, this))
    }
    //    cells.foreach(c => println(c.dx + "," + c.dy))
    def canMoveTo(dx: Int, dy: Int): Boolean =
      cells.find(cell => cell.dx == dx && cell.dy == dy).map(_.isAccessible).getOrElse(false)
  }
  // dx and dy are the deltas to move from the MasterBot's position (0, 0)
  case class Cell(s: String, dx: Int, dy: Int, view: View) {
    val points: Int = {
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
    def isAccessible = isEmpty || isMyMiniBot || isZugar || isFluppet || isEnemyMiniBot || isMaster
    def isMyMiniBot = s == "S"
    def isWall = s == "W"
    def isEmpty = s == "_"
    def isZugar = s == "P"
    def isFluppet = s == "B"
    def isEnemyMiniBot = s == "s"
    def isEnemy = s == "m"
    def isToxifera = s == "p"
    def isSnorg = s == "b"
    def isMaster = s == "M"
    def isUnknown = s == "?"
  }
  case class MoveTo(cell: Cell) {
    def toResponse: String = {
      val response = "Move(dx=%s,dy=%s)".format(cell.dx, cell.dy)
      println(response)
      response
    }
  }
  case class Navigator(view: View) {
    val whereIAm = view.cells.find(c => c.dx == 0 && c.dy == 0).get
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
      def addAllVertices: Unit = view.cells.foreach(graph.addVertex(_))
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
          view.cells.filter(isNeighbor)
        }
        for {
          v <- view.cells
          neighbor <- neighborsOf(v)
          _ = graph.addEdge(v, neighbor)
        } yield ()
      }
      addAllVertices
      addAllEdges
      graph
    }
    type Fitness = Double
    implicit val moveOrdering = new Ordering[(Cell, Fitness)] {
      override def compare(a: (Cell, Fitness), b: (Cell, Fitness)) = a._2.compareTo(b._2)
    }
    def explore = MoveTo(whereIAm) // TODO
    def bestMove: MoveTo = {
      val cellsWithBenefit = view.cells.filter(_.points > 0)
      if (cellsWithBenefit.isEmpty) {
        implicit def shuffle(l: Seq[Cell]) = new {
          def shuffle: Seq[Cell] = {
            import scala.collection.JavaConverters._
            import java.util._
            val jList = new ArrayList(l.asJava)
            Collections.shuffle(jList)
            jList.asScala
          }
        }
        // TODO: something more sophisticated than a random jump
        val destination = view.cells.filter(_.isAccessible).shuffle(0)
        createMove(destination)
      } else {
        val cellsWithFitness = cellsWithBenefit.map(c => (c, fitness(c)))
        val (destination, withHighestPayoff) = cellsWithFitness.max
        createMove(destination)
      }
    }
    private def createMove(destination: Cell): MoveTo = {
      // theoretically this could return null but I don't think that will happen 
      // given that the graph should only consist of accessible points
      import org.jgrapht.alg._
      import scala.collection.JavaConverters._
      val bestPath = DijkstraShortestPath.findPathBetween(graph, whereIAm, destination)
      val Edge(src, dest) = bestPath.asScala(0)
      MoveTo(dest)
    }
    private def fitness(cell: Cell): Fitness = {
      // should also tak into consideration the distance between the bot and the cell
      def distance: Double = euclideanDistance((0, 0), (cell.dx, cell.dy))
      def normalized(points: Int): Double = points / distance
      normalized(cell.points)
    }
  }

}