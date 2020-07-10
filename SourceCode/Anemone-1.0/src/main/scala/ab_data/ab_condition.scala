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
     Conditions
     ----------
*/

abstract class AG_COND {
   def ab_toString: String = { " " }
   def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND
   def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND      
}

case class COND_ATOMIC(relop: String,argi:AB_SI_ELM,argii:AB_SI_ELM) extends AG_COND {
   override def ab_toString: String = { argi.ab_toString + relop + argii.ab_toString }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
     COND_ATOMIC(relop,argi.ab_substitute(xs,ts),argii.ab_substitute(xs,ts)) }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
     COND_ATOMIC(relop,argi.ab_simplify(xs,ts),argii.ab_simplify(xs,ts)) }   
}

case class COND_COMPI(boolop: String,arg:AG_COND) extends AG_COND {
   override def ab_toString: String = { boolop + arg.ab_toString }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
      COND_COMPI(boolop,arg.ab_substitute(xs,ts)) }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
      COND_COMPI(boolop,arg.ab_simplify(xs,ts)) }
}

case class COND_COMPII(boolop: String,argi:AG_COND,argii:AG_COND) extends AG_COND {
   override def ab_toString: String = { argi.ab_toString + boolop + argii.ab_toString }
   override def ab_substitute(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
      COND_COMPII(boolop,argi.ab_substitute(xs,ts),argii.ab_substitute(xs,ts)) }
   override def ab_simplify(xs:List[AB_SI_ELM],ts:List[AB_SI_ELM]):AG_COND = {
      COND_COMPII(boolop,argi.ab_simplify(xs,ts),argii.ab_simplify(xs,ts)) }
}

