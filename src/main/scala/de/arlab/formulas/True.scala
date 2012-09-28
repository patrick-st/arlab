package de.arlab.formulas

/**
 * repräsentiert True
 */
 case object True extends Formula {

  /**
   * Gibt die Menge der freien Variablen zurück
   * @return
   */
  def free: Set[Variable] = Set[Variable]()

  /**
   * Gibt die Menge der gebundenen Variablen zurück
   * @return
   */
  def bound: Set[Variable] = Set[Variable]()

  /**
   * substituiert alle Terme, die über die Map sfn definiert sind
   * @param sfn
   * @return
   */
  def subst(sfn: Map[Variable,Term]): Formula = this

  /**
   * Gibt ale Funktionen (mit Stelligkeit) zurück
   * @return
   */
  def functions: Set[(String, Int)]  = Set[(String, Int)]()
}
