/***********************************************************************

 Copyright (c) 2019 Jean-Marie Jacquet and the CoordiNam Lab members
 (University of Namur, Belgium)
 
 Permission is hereby granted, free of charge, to any person obtaining
 a  copy of  this  software and  associated  documentation files  (the
 "Software"), to  deal in the Software  without restriction, including
 without limitation the  rights to use, copy,  modify, merge, publish,
 distribute, sublicense,  and/or sell copies  of the Software,  and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The  above  copyright notice  and  this  permission notice  shall  be
 included in all copies or substantial portions of the Software.
 
 THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
 EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
 MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
 NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
 BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
 ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
 CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
 SOFTWARE.
 
************************************************************************/


package ab_data


/*
     Structured information
     ----------------------
*/

class AB_SI_DECL
case class AB_SI_ATOMIC_DECL(nameSI: String) extends AB_SI_DECL
case class AB_SI_COMPOSED_DECL(functorName: String, functorArgs: List[(AB_SET_EXPR,AB_SET_EXPR)]) extends AB_SI_DECL

abstract class AB_SI_ELM {
   def ab_toString: String = { " " }
   def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM
   def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM
   def atomicSiElm:Boolean
   def functorSiElm:String
}

case class AB_SI_ATOMIC(functor: String) extends AB_SI_ELM {
   override def ab_toString: String = { functor } 
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM = {
       if (xs.contains(AB_SI_ATOMIC(functor))) {
         val i = xs.indexOf(AB_SI_ATOMIC(functor))
	 ts(i)
       } else { AB_SI_ATOMIC(functor)
       }
   }
  override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM   = {
       if (xs.contains(AB_SI_ATOMIC(functor))) {
         val i = xs.indexOf(AB_SI_ATOMIC(functor))
	 ts(i)
       } else { AB_SI_ATOMIC(functor)
       }
  }
  override def atomicSiElm = { true }
  override def functorSiElm = { functor }
}

case class AB_SI_COMPOSED(functor: String, functorArgs: List[AB_SI_ELM]) extends AB_SI_ELM {
   override def ab_toString: String = {
      var s = ""
      for (a <- functorArgs) { s = s + a.ab_toString + "," }
      s = s.substring(0, s.length() - 1)
      functor + "(" + s + ")"
   }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM = {
      AB_SI_COMPOSED(functor,functorArgs.map(e=>e.ab_substitute(xs,ts)))
   }
  override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SI_ELM   = {
      val fArgsSimplified = functorArgs.map(x=>x.ab_simplify(xs,ts))
      if (xs.contains(AB_SI_COMPOSED(functor,fArgsSimplified))) {
         val i = xs.indexOf(AB_SI_COMPOSED(functor,fArgsSimplified))
	 ts(i)
      } else { AB_SI_COMPOSED(functor,fArgsSimplified)
      }
  }
  override def atomicSiElm = { false }
  override def functorSiElm = { functor }  
}



/*
     Signed structured information
     -----------------------------
*/

abstract class AB_SIGN {
   def ab_toString: String = { " " }
}

case class ab_sign_plus() extends AB_SIGN {
   override def ab_toString: String = { "+" } 
}

case class ab_sign_minus() extends AB_SIGN {
   override def ab_toString: String = { "-" } 
}


abstract class AB_SIGNED_SI_PN_ELM {
   def ab_toString: String = { " " }
   def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM
   def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM
}


/*    for passive si-terms
*/

case class AB_SSI_ELM(s:AB_SIGN,si_elm:AB_SI_ELM) extends AB_SIGNED_SI_PN_ELM {
   override def ab_toString: String = { s.ab_toString + si_elm.ab_toString }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM = {
       AB_SSI_ELM(s,si_elm.ab_substitute(xs,ts))
   }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM = {
       AB_SSI_ELM(s,si_elm.ab_simplify(xs,ts))
   }
}


/*   for active procedure calls
*/

case class AB_SPC_ELM(s:AB_SIGN,pn:String,args:List[AB_SI_ELM]) extends AB_SIGNED_SI_PN_ELM {
   override def ab_toString: String = { s.ab_toString + pn + "(" + args.map(e=>e.ab_toString).mkString(",") + ")"}
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM = {
       AB_SPC_ELM(s,pn,args.map(e=>e.ab_substitute(xs,ts)))
   }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AB_SIGNED_SI_PN_ELM = {
       AB_SPC_ELM(s,pn,args.map(e=>e.ab_simplify(xs,ts)))
   }
}



/*
     Structured information with variables
     -------------------------------------

     used for parsing in interactive blackboard

*/

abstract class AB_VAR_SI_ELM {
   def ab_toString: String = { " " }
}

case class AB_VAR_ELM(varName: String) extends AB_VAR_SI_ELM {
   override def ab_toString: String = { varName }
}

case class AB_VAR_SI_TERM(si: AB_SI_ELM) extends AB_VAR_SI_ELM {
   override def ab_toString: String = { si.ab_toString }
}

case class AB_VAR_SI_COMPOSED(functor: String, functorArgs: List[AB_VAR_SI_ELM]) extends AB_VAR_SI_ELM {
   override def ab_toString: String = {
      var s = ""
      for (a <- functorArgs) { s = s + a.ab_toString + "," }
      s = s.substring(0, s.length() - 1)
      functor + "(" + s + ")"
   }
}


/*
     Vars in Sets
     ------------

     declarations of var with sets

*/

abstract class AB_GEN_VAR_IN_SET {
   def ab_toString: String = { " " }
   def ab_varN: String = " "
   def ab_sname : String = " "
}

case class AB_VAR_IN_SET(ab_var:String,ab_set_name:String) extends AB_GEN_VAR_IN_SET {
   override def ab_toString:String = { ab_var + " in " + ab_set_name }
   override def ab_varN = ab_var
   override def ab_sname = ab_set_name
}


/*
     General functions for si-terms
     ------------------------------

     See file ab_package_object.scala
     
*/

