package de.arlab.formulas

/**
 * repräsentiert die logische Negation
 * @param a entspricht einer Formel
 */
case class Not(a: Formula) extends Formula {

  override def toString = {
      "~" + a
  }

  /**
   * Bestimmung der freien Variablen der Formeln a
   * @return
   */
  def free: Set[Variable] = a.free

  /**
   * Bestimmung der gebundenen Variablen der Formeln a
   * @return
   */
  def bound: Set[Variable] = a.bound

  def subst(sfn: Map[Variable,Term]): Formula = new Not(a.subst(sfn))


}
