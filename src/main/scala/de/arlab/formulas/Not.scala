package de.arlab.formulas

/**
 * repräsentiert die logische Negation
 * @param a entspricht einer Formel
 */
case class Not(a: Formula) extends Formula {

  override def toString = {
      "~" + a
  }

}
