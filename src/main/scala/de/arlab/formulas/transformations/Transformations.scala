package de.arlab.formulas.transformations

import de.arlab.formulas._


object Transformations {

  /**
   * erzeugt die NNF einer Formel
   * @param fm übergebene Formel
   * @return
   */
  def nnf(fm: Formula) : Formula = fm match{
    case Not(ForAll(x,a)) => Exists(x,nnf(Not(a)))
    case Not(Exists(x,a)) => ForAll(x,nnf(Not(a)))
    case Not(And(a,b)) => Or(nnf(Not(a)),nnf(Not(b)))
    case Not(Or(a,b)) => And(nnf(Not(a)),nnf(Not(b)))
    case Not(Not(a)) => nnf(a)
    case Not(True) => False
    case Not(False) => True
    case And(a,b) => And(nnf(a), nnf(b))
    case Or(a,b) => Or(nnf(a), nnf(b))
    case ForAll(x,a) => ForAll(x,nnf(a))
    case Exists(x,a) => Exists(x,nnf(a))
    case other => other
  }

  /**
   * erzeugt die CNF einer Formel
   * @param fm übergebene Formel
   * @return
   */
  def cnf(fm: Formula) : Formula = {
    val nnfFm = Transformations.nnf(fm)
    nnfFm match{
      case Or(a,And(b,c)) => And(cnf(Or (a,b)),cnf(Or(a,c)))
      case Or(And(b,c),a) => And(cnf(Or(b,a)),cnf(Or(c,a)))
      case And(a,b) => And(cnf(a),cnf(b))
      case other => other
    }
  }

  /**
   * vereinfacht Formeln schrittweise
   * @param fm
   * @return
   */
   def presimplify(fm : Formula) : Formula = fm match {
     //Absorptionsregeln
    /* case Or(a,And(b,c)) if b == a || a == c => presimplify(a)
     case Or(And(b,c),a) if a == b || a == c => presimplify(a)
     case And(a,Or(b,c)) if b == a || c == a  => presimplify(a)
     case And(Or(b,c),a) if a == b || a == c => presimplify(a)  */

     case And(a,b) if a == b => presimplify(a)
     case And(a,b) if b == Not(a) || a == Not(b) => False
     case And(a,b) if a == True => presimplify(b)
     case And(a,b) if b == True => presimplify(a)
     case And(a,b) if a == False || b == False => False
     case And(a,b)  => And(presimplify(a), presimplify(b))

     case Or(a,b) if a == b => presimplify(a)
     case Or(a,b) if b == Not(a) || a == Not(b) => True
     case Or(a,b) if a == True || b == True => True
     case Or(a,b) if a == False => presimplify(b)
     case Or(a,b) if b == False => presimplify(a)
     case Or(a,b)  if a == False && b == False => False
     case Or(a,b)  => Or(presimplify(a), presimplify(b))

     case Not(Not(a)) => presimplify(a)

     case other => other



   }

  /**
   * reduziert eine quantorenfreie Formel so weit wie möglich
   * @param fm
   * @return
   */
  def simplify(fm: Formula) : Formula = {
    var changed = true
    var f = fm
   // erste Vereinfachung der Formel
    var simpfm = Transformations.presimplify(f)
    /*Die Formel wird solange vereinfacht, bis sie sich nicht mehr ändert
     * anschließend  wird die vereinfachte Formel ausgegeben
     */
    while(changed){
      if (f == simpfm) {
          changed = false
      }  else {
        f = simpfm
        simpfm = Transformations.presimplify(simpfm)
      }
    }
    return simpfm
  }

  /**
   *   skolemisiert die Formel fm
   * @param fm
   * @return
   */
  def skolemize(fm: Formula): Formula = {
    var nnfFm = Transformations.nnf(fm)
    nnfFm match {
      case ForAll(x,a) => ForAll(x,skolemize(a))
      case Exists(x,a) => {
        val free = nnfFm.free
        val skolem = Function("sk", free.toSeq:_*).variant(a.functions)
        skolemize(a.subst(Map(x -> skolem)))
      }
      case And(a,b) => And(skolemize(a),skolemize(b))
      case Or(a,b) => Or(skolemize(a), skolemize(b))

      case other => other
    }
  }

  /**
   * erzeugt die Pränexnormalform der Formel fm
   * @param fm
   * @return
   */
    def pnf(fm: Formula): Formula = {
     val allVariables = fm.free union fm.bound
      fm match{
        case And(a,ForAll(x,y)) if(a.free.contains(x)) => {
          val variant = x.variant(allVariables)
          val map = Map(x -> variant)
          pnf(ForAll(variant,And(a,y.subst(map))))
        }
        case And(a,ForAll(x,y)) if(!a.free.contains(x)) => pnf(ForAll(x,And(a,y)))

        case Or(a,ForAll(x,y)) if(a.free.contains(x)) => {
          val variant = x.variant(allVariables)
          val map = Map(x -> variant)
          pnf(ForAll(variant,Or(a,y.subst(map))))
        }
        case Or(a,ForAll(x,y)) if(!a.free.contains(x)) => pnf(ForAll(x,Or(a,y)))
        case ForAll(x,a) => ForAll(x,pnf(a))
        case other => other

      }

    }

  /**
   * Rückgabe der Matrix einer quantorenfreien PNF
   * @param fm
   * @return
   */
  def matrix(fm: Formula): Formula = fm match{
    case ForAll(x,a) => matrix(a)
    case Exists(x,a) => matrix(a)
    case other => other
  }

  /**
   * allquantifiziert alle freien Variablen
   * @param fm
   * @return
   */
  def generalize(fm: Formula): Formula = {
    val free = fm.free
    if(!free.isEmpty){
      val first = free.head
      val fm_ = ForAll(first,fm)
      generalize(fm_)
    } else fm
  }



}
