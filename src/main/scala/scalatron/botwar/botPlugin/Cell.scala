package scalatron.botwar.botPlugin

/**
 * dx and dy are the deltas to move from the MasterBot's position (0, 0)
 */
case class Cell(s: String, dx: Int, dy: Int) {
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