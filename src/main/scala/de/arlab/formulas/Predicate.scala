package de.arlab.formulas

/**
 * repräsentiert Prädikate
 * @param name
 * @param terms  Terme des Prädikats
 */
case class Predicate(name: String, terms: Term*) extends Formula {

  override def toString = {
    name + "(" + terms.mkString(",") + ")"
  }
}
