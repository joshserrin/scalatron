package scalatron.botwar

package object botPlugin {
  def distance(origin: Cell, destination: Cell): Double =
    euclideanDistance((origin.dx, origin.dy), (destination.dx, destination.dy))
  def euclideanDistance(a: (Int, Int), b: (Int, Int)): Double = {
    val (p1, p2) = (a._1, a._2)
    val (q1, q2) = (b._1, b._2)
    implicit def squared(d: Int) = new { def squared: Int = d * d }
    Math.sqrt((p1 - q1).squared + (p2 - q2).squared)
  }
  implicit def shuffle(l: Seq[Cell]) = new {
    def shuffle: Seq[Cell] = {
      import java.util._
      import scala.collection.JavaConverters._
      val jlist = new ArrayList(l.asJava)
      Collections.shuffle(jlist)
      jlist.asScala
    }
  }
}