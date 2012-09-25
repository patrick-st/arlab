package de.arlab.formulas

/**
 * repräsentiert den Existenzquantor
 * @param variable
 * @param a entspricht einer Formel
 */
case class Exists(variable: Variable, a: Formula) extends Formula {

}
