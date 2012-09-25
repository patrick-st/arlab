package de.arlab.formulas

/**
 *  repräsentiert das logische Und
 * @param a entspricht einer Formel
 * @param b entspricht einer Formel
 */
case class And(a: Formula, b: Formula) extends Formula {

  override def toString =
  "(" + a + " & " + b + ")"
}
