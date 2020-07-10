/***********************************************************************

 Copyright (c) 2020 Jean-Marie Jacquet and the CoordiNam Lab members
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

package ab_blackboard

import ab_data._
import ab_scene._

import scala.collection.mutable.Map

class ABRules {

   var ab_theRulesDecl = Map[String,AB_RULE]()
   // map to register rule declaration per rule Name
   var ab_nbOccRule = Map[String,Int]()
   // a rule can be told several times. This map registers for each rule
   // being told the current number of occurrences
   var ab_theRulesUnderExecution = Map[String,List[AB_GEN_SPEC_RULE]]()
   // to each rule name is associated the list of rule instantiations


   def ab_existRuleUE: Boolean = {
      !ab_theRulesUnderExecution.isEmpty
   }

   def addRuleDecls(d: List[AB_GEN_RULE]) = {
      for (e <- d) {
        e match {
         case  AB_RULE(ruleName, varsInFor, pre, post) =>
	   { if (ab_theRulesDecl.contains(ruleName)) {
                ab_theRulesDecl(ruleName) = AB_RULE(ruleName, varsInFor, pre, post) 
	     } else {
                ab_theRulesDecl ++= Map(ruleName -> AB_RULE(ruleName, varsInFor, pre, post))
	     }
	   }
        }
      }
   }


   def cross_product_set_elms(lst: List[List[String]],acc:List[String]): List[List[String]] = {
      lst match {
        case head :: Nil => head.map(_ :: acc)
        case head :: tail => head.flatMap(x => cross_product_set_elms(tail,x :: acc))
        case Nil => List[List[String]]()
      }
   }

   def list_rule_instances(rule:AB_RULE,bb: ABBlackboard): List[AB_GEN_SPEC_RULE] = {
     rule match {
       case AB_RULE(ruleName,varsInFor,pre,post) => {
         varsInFor match {
	   case Nil => List(AB_SPEC_RULE(ruleName, pre, post))
	   case x :: xs => {
             val varsInForAsString = rule.ab_listVars
             val varsInForAsSiElms = varsInForAsString.map(x=>AB_SI_ATOMIC(x))
             val list_set_names = rule.ab_list_set_names
             val ll_selms = list_set_names.map(sn=>bb.setMembers(sn))
             val llElmsInForAsString = cross_product_set_elms(ll_selms,List[String]())
             var preInst = List[AB_SIGNED_SI_PN_ELM]()
             var postInst = List[AB_SIGNED_SI_PN_ELM]()
             var rulesInst = List[AB_GEN_SPEC_RULE]()
             var eAsSiElms = List[AB_SI_ELM]()
             for (e <- llElmsInForAsString) {
               eAsSiElms = e.map(x=>AB_SI_ATOMIC(x))
               preInst = pre.map(x=>x.ab_substitute(varsInForAsSiElms,eAsSiElms))
               postInst = post.map(x=>x.ab_substitute(varsInForAsSiElms,eAsSiElms))
               rulesInst = AB_SPEC_RULE(ruleName, preInst, postInst) :: rulesInst
             }
             rulesInst
	   }
         }
       }
     }
   }

   def test_tellr(ruleName:String): Boolean = true

   def tellr(ruleName:String,bb:ABBlackboard): Boolean = {
      if (ab_theRulesDecl.contains(ruleName)) {
        if (ab_nbOccRule.contains(ruleName)) {
	  ab_nbOccRule(ruleName) += 1
	} else {
	  ab_nbOccRule ++= Map(ruleName -> 1)	
	}
      }

      if ( ab_nbOccRule(ruleName) == 1 ) {
        val lRuleInstances = list_rule_instances(ab_theRulesDecl(ruleName),bb)
        if (ab_theRulesUnderExecution.contains(ruleName)) {
	     ab_theRulesUnderExecution(ruleName) = lRuleInstances
	} else {
	     ab_theRulesUnderExecution ++= Map(ruleName -> lRuleInstances)
	}
      }

      true
   }

   def test_askr(ruleName:String): Boolean = {
      if (ab_nbOccRule.contains(ruleName)) {
        if (ab_nbOccRule(ruleName) >= 1) { true
	} else { false
	}
      } else {
        false
      }	
   }

   def askr(ruleName:String): Boolean = {
      if (ab_nbOccRule.contains(ruleName)) {
        if (ab_nbOccRule(ruleName) >= 1) { true
	} else { false
	}
      } else {
        false
      }	
   }

   def test_getr(ruleName:String): Boolean = test_askr(ruleName)
   
   def removeRuleInst(ruleName:String) {
      if (ab_theRulesUnderExecution.contains(ruleName)) {
         ab_theRulesUnderExecution -= ruleName
      }
   }
   
   def getr(ruleName:String): Boolean = {
      if (ab_nbOccRule.contains(ruleName)) {
        if (ab_nbOccRule(ruleName) >= 1) {
          ab_nbOccRule(ruleName) = ab_nbOccRule(ruleName) - 1
	  if (ab_nbOccRule(ruleName) == 0) { removeRuleInst(ruleName) }
          true
	} else { false
	}
      } else {
        false
      }	
   }

   def test_naskr(ruleName:String): Boolean = {
      if (ab_nbOccRule.contains(ruleName)) {
        if (ab_nbOccRule(ruleName) >= 1) { false
	} else { true
	}
      } else {
        true
      }	
   }

   def naskr(ruleName:String): Boolean = {
      if (ab_nbOccRule.contains(ruleName)) {
        if (ab_nbOccRule(ruleName) >= 1) { false
	} else { true
	}
      } else {
        true
      }	
   }


   def posSITerms(pre: List[AB_SIGNED_SI_PN_ELM]): Map[AB_SI_ELM,Int] = {
      var m = Map[AB_SI_ELM,Int]()
      for (e <- pre) {
         e match {
            case AB_SSI_ELM(s,si_elm) => {
               s match {
                  case ab_sign_plus() => {
                    if (m.contains(si_elm)) {
		      m(si_elm) = m(si_elm) + 1
		    } else {
                      m = m ++ Map(si_elm -> 1)
                    }
                  }
  	          case ab_sign_minus() => 
               }
            }
            case AB_SPC_ELM(s,pn,args) => 
         }
      }
      m
   }
   
   def negSITerms(pre: List[AB_SIGNED_SI_PN_ELM]): List[AB_SI_ELM] = {
      var l = List[AB_SI_ELM]()
      for (e <- pre) {
         e match {
            case AB_SSI_ELM(s,si_elm) => {
               s match {
                  case ab_sign_plus() => 
  	          case ab_sign_minus() => { l = si_elm :: l }
               }
            }
            case AB_SPC_ELM(s,pn,args) => 
         }
      }
      l
   }
   
   def posPCalls(pre: List[AB_SIGNED_SI_PN_ELM]): Map[String,Int] = {
      var m = Map[String,Int]()
      for (e <- pre) {
         e match {
            case AB_SSI_ELM(s,si_elm) => 
            case AB_SPC_ELM(s,pn,args) => {
   	       val proc_call = pn + "(" + args.map(e=>e.ab_toString).mkString(",") + ")"
               s match {
                  case ab_sign_plus() => {
                    if (m.contains(proc_call)) {
		      m(proc_call) = m(proc_call) + 1
		    } else {
                      m = m ++ Map(proc_call -> 1)
                    }
                  }
   	          case ab_sign_minus() =>
               }
            }
         }
      }
      m
   }
   
   def negPCalls(pre: List[AB_SIGNED_SI_PN_ELM]): List[String] = {
      var l = List[String]()
      for (e <- pre) {
         e match {
            case AB_SSI_ELM(s,si_elm) => 
            case AB_SPC_ELM(s,pn,args) => {
   	       val proc_call = pn + "(" + args.map(e=>e.ab_toString).mkString(",") + ")"
               s match {
                  case ab_sign_plus() => 
   	          case ab_sign_minus() => { l = proc_call :: l }
               }
            }
         }
      }
      l
   }


   def list_rules_under_execution = ab_theRulesUnderExecution


}

