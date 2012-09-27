package de.arlab.formulas

/**
 *  repräsentiert das logische Und
 * @param a entspricht einer Formel
 * @param b entspricht einer Formel
 */
case class And(a: Formula, b: Formula) extends Formula {

  override def toString =
  "(" + a + " & " + b + ")"

  /**
   * Bestimmung der freien Variablen der Formeln a und b
   * anschließende Vereinigung der beiden Mengen
   * @return
   */
  def free: Set[Variable] = a.free union b.free

  /**
   * Bestimmung der gebundenen Variablen der Formeln a und b
   * anschließende Vereinigung der beiden Mengen
   * @return
   */
  def bound: Set[Variable] = a.bound union b.bound

  def subst(sfn: Map[Variable,Term]): Formula = new And(a.subst(sfn), b.subst(sfn))
}
