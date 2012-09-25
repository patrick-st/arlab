package de.arlab.formulas

/**
 *  repräsentiert das logische Oder
 * @param a entspricht einer Formel
 * @param b entspricht einer Formel
 */
 case class Or(a: Formula, b: Formula) extends Formula {

  override def toString =
    "("+ a + " | " + b + ")"
}
