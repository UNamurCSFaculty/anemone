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

import ab_blackboard._


/*  Note : definitions of signed structured information
    see file ab_struct_info.scala
*/



/*
      Rule: general form
      ------------------
*/
      
abstract class AB_GEN_RULE {
   def ab_toString: String = { " " }
   def list_rule_instances(bb: ABBlackboard) = List[AB_GEN_SPEC_RULE]()
}

case class AB_RULE(ruleName: String,
                   varsInFor:List[AB_GEN_VAR_IN_SET],
                   pre:List[AB_SIGNED_SI_PN_ELM],post:List[AB_SIGNED_SI_PN_ELM])
		          extends AB_GEN_RULE {
   override def ab_toString: String = {
        ruleName +
        "For " + varsInFor.map(e=>e.ab_toString).mkString(", ") + ": "
        pre.map(e=>e.ab_toString).mkString(", ") + " --> " +
        post.map(e=>e.ab_toString).mkString(", ")
   }

   def ab_listVars:List[String] = { varsInFor.map(x=>x.ab_varN)
   }

   def ab_list_set_names: List[String] = {
      varsInFor.map(x=>x.ab_sname)  
   }
   

}



/*
      Rule: specialized form
      ----------------------
*/


abstract class AB_GEN_SPEC_RULE {
   def ab_toString: String = { " " }
}

case class AB_SPEC_RULE(parentRuleName: String,
                        pre:List[AB_SIGNED_SI_PN_ELM],
			post:List[AB_SIGNED_SI_PN_ELM])
		                         extends AB_GEN_SPEC_RULE {
   override def ab_toString: String = {
        "From " + parentRuleName + ":  " +
	pre.map(e=>e.ab_toString).mkString(", ") + " --> " +
        post.map(e=>e.ab_toString).mkString(", ")
   }
}
