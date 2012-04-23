package scalatron.botwar.botPlugin

/**
 * dx and dy are the deltas to move from the MasterBot's position (0, 0)
 */
case class Cell(s: String, dx: Int, dy: Int, view: View) {
  /**
   * the number of EU's to be received if the Master lands on this cell
   */
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
  /**
   * True if this cell represents something that wouldn't reduce the number of
   * EUs or bonk the Master
   */
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