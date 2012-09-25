package de.arlab.formulas

/**
 *  repräsentiert eine Variable
 * @param value Variablenbezeichnung
 */
case class Variable(value: String) extends Term {
  override def toString = value

}
